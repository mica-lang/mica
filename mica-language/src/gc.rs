//! Garbage collection.

use std::alloc::{handle_alloc_error, Layout};
use std::borrow::Borrow;
use std::cell::Cell;
use std::fmt;
use std::mem::ManuallyDrop;
use std::ops::Deref;

use crate::bytecode::DispatchTable;
use crate::value::{Closure, RawValue, Struct, UserData, ValueKind};

/// A garbage collected pool of a specific type of data.
pub struct Memory {
   strings: Vec<GcRaw<String>>,
   closures: Vec<GcRaw<Closure>>,
   structs: Vec<GcRaw<Struct>>,
   user_data: Vec<GcRaw<Box<dyn UserData>>>,
   dispatch_tables: Vec<GcRaw<DispatchTable>>,
}

impl Memory {
   /// Creates a new GC.
   pub fn new() -> Self {
      Self {
         strings: Vec::new(),
         closures: Vec::new(),
         structs: Vec::new(),
         user_data: Vec::new(),
         dispatch_tables: Vec::new(),
      }
   }

   /// Marks and sweeps unused allocations.
   ///
   /// # Safety
   /// All root pointers in values yielded by the iterator must be valid.
   pub(crate) unsafe fn collect(&mut self, roots: impl Iterator<Item = RawValue>) {
      unsafe fn mark_all_unreachable<T>(memories: impl Iterator<Item = GcRaw<T>>) {
         for memory in memories {
            let mem = memory.get_mem();
            mem.reachable.set(false);
         }
      }

      unsafe fn mark_dtable_reachable_rec(mem: GcRaw<DispatchTable>) {
         if !mem.get_mem().reachable.get() {
            mem.mark_reachable();
            let dtable = mem.get();
            if let Some(instance) = dtable.instance {
               mark_dtable_reachable_rec(instance);
            }
            for method in dtable.methods() {
               mark_reachable_rec(RawValue::from(method))
            }
         }
      }

      unsafe fn mark_reachable_rec(value: RawValue) {
         match value.kind() {
            ValueKind::Nil | ValueKind::Boolean | ValueKind::Number => (),
            ValueKind::String => {
               let raw = value.get_raw_string_unchecked();
               raw.mark_reachable();
            }
            ValueKind::Function => {
               let raw = value.get_raw_function_unchecked();
               if !raw.get_mem().reachable.get() {
                  raw.mark_reachable();
                  let closure = raw.get();
                  for upvalue in &closure.captures {
                     mark_reachable_rec(upvalue.get());
                  }
               }
            }
            ValueKind::Struct => {
               let raw = value.get_raw_struct_unchecked();
               if !raw.get_mem().reachable.get() {
                  raw.mark_reachable();
                  let struct_v = raw.get();
                  for field in struct_v.fields() {
                     mark_reachable_rec(field);
                  }
                  let dtable = *raw.get().dtable.get();
                  mark_dtable_reachable_rec(dtable);
               }
            }
            // TODO: Allow user data to specify its own references.
            ValueKind::UserData => {
               let raw = value.get_raw_user_data_unchecked();
               if !raw.get_mem().reachable.get() {
                  raw.mark_reachable();
               }
            }
         }
      }

      unsafe fn sweep_unreachable<T>(memories: &mut Vec<GcRaw<T>>) {
         let mut i = 0;
         while i < memories.len() {
            let memory = memories[i];
            let mem = memory.get_mem();
            if !mem.reachable.get() {
               GcMem::release(memory);
               memories.swap_remove(i);
            } else {
               i += 1;
            }
         }
      }

      // NOTE: Marking all objects as unreachable beforehand is *somehow* faster than doing it
      // during the sweep phase. I believe it might have something to do with the objects being
      // loaded into the CPU cache but I'm really not sure.
      mark_all_unreachable(self.strings.iter().copied());
      mark_all_unreachable(self.closures.iter().copied());
      mark_all_unreachable(self.structs.iter().copied());
      mark_all_unreachable(self.user_data.iter().copied());
      mark_all_unreachable(self.dispatch_tables.iter().copied());
      for value in roots {
         mark_reachable_rec(value);
      }
      sweep_unreachable(&mut self.strings);
      sweep_unreachable(&mut self.closures);
      sweep_unreachable(&mut self.structs);
      sweep_unreachable(&mut self.user_data);
      sweep_unreachable(&mut self.dispatch_tables);
   }

   /// Performs an _automatic_ collection.
   ///
   /// Automatic collections only trigger upon specific conditions, such as a specific amount of
   /// generations passing. Though right now there are no such conditions.
   pub(crate) unsafe fn auto_collect(&mut self, roots: impl Iterator<Item = RawValue>) {
      self.collect(roots)
   }

   /// Allocates a new `GcRaw<T>` managed by this GC.
   pub fn allocate<T>(&mut self, data: T) -> GcRaw<T>
   where
      T: GarbageCollected,
   {
      let gcmem = GcMem::allocate(data);
      T::allocate_in_gc(gcmem, self);
      gcmem
   }

   /// If the provided `Gc<T>` isn't managed by a GC, makes it managed by this GC.
   /// Otherwise does nothing.
   pub fn manage<T>(&mut self, gc: &Gc<T>) -> GcRaw<T>
   where
      T: GarbageCollected,
   {
      unsafe {
         let mem = gc.mem.get_mem();
         if !mem.managed_by_gc.get() {
            T::allocate_in_gc(gc.mem, self);
            mem.managed_by_gc.set(true);
         }
         gc.mem
      }
   }
}

impl Default for Memory {
   fn default() -> Self {
      Self::new()
   }
}

impl Drop for Memory {
   fn drop(&mut self) {
      unsafe { self.collect(std::iter::empty()) }
   }
}

/// Implemented by all types that can be garbage collected.
pub trait GarbageCollected {
   fn allocate_in_gc(mem: GcRaw<Self>, gc: &mut Memory)
   where
      Self: Sized;
}

macro_rules! implement_garbage_collected {
   ($T:ty, $field:tt) => {
      impl GarbageCollected for $T {
         fn allocate_in_gc(mem: GcRaw<Self>, gc: &mut Memory)
         where
            Self: Sized,
         {
            gc.$field.push(mem);
         }
      }
   };
}

implement_garbage_collected!(String, strings);
implement_garbage_collected!(Closure, closures);
implement_garbage_collected!(Struct, structs);
implement_garbage_collected!(Box<dyn UserData>, user_data);
implement_garbage_collected!(DispatchTable, dispatch_tables);

/// An allocation with metadata.
#[repr(align(8))]
pub(crate) struct GcMem<T> {
   /// Whether the memory is still reachable.
   /// This is used by the mark phase to determine which memories should (not) be swept.
   reachable: Cell<bool>,
   /// Whether the memory is still being managed by the garbage collector.
   managed_by_gc: Cell<bool>,
   /// Foreign references to this memory.
   rc: Cell<usize>,
   /// The data.
   data: ManuallyDrop<T>,
}

impl<T> GcMem<T> {
   /// Returns the allocation layout of a `GcMem<T>`.
   fn layout() -> Layout {
      Layout::new::<Self>()
   }

   /// Allocates a `GcMem<T>`.
   fn allocate(data: T) -> GcRaw<T> {
      let mem = Self {
         // NOTE: `reachable` is initially set to `false` because reachability is only determined
         // during the marking phase.
         reachable: Cell::new(false),
         managed_by_gc: Cell::new(true),
         rc: Cell::new(0),
         data: ManuallyDrop::new(data),
      };
      let layout = Self::layout();
      let allocation = unsafe { std::alloc::alloc(layout) } as *mut Self;
      if allocation.is_null() {
         handle_alloc_error(layout);
      }
      unsafe { *allocation = mem }
      #[cfg(feature = "trace-gc")]
      {
         println!("gcmem | allocated {:p}", allocation);
      }
      GcRaw(allocation as *const _)
   }

   /// Deallocates a `GcMem<T>`.
   ///
   /// # Safety
   /// `mem` must be a pointer returned by [`allocate`][`Self::allocate`].
   unsafe fn deallocate(mem: GcRaw<T>) {
      let mem = mem.0 as *mut GcMem<T>;
      #[cfg(feature = "trace-gc")]
      {
         println!("gcmem | deallocating {:p}", mem);
      }
      {
         let mem = &mut *mem;
         ManuallyDrop::drop(&mut mem.data);
      }
      // Ugh, that cast from *const to *mut hurts.
      std::alloc::dealloc(mem as *mut u8, Self::layout())
   }

   /// Deallocates the given memory or marks it as unmanaged if there are foreign references to it.
   unsafe fn release(memory: GcRaw<T>) {
      #[cfg(feature = "trace-gc")]
      {
         println!("gcmem | releasing {:p}", memory.0);
      }
      let mem = memory.get_mem();
      if mem.rc.get() > 0 {
         mem.managed_by_gc.set(false);
      } else {
         GcMem::deallocate(memory);
      }
   }
}

/// An unmanaged reference to GC memory.
///
/// Be careful around this, as values of this type must not outlive the [`Memory`] they were born
/// in.
#[derive(Debug)]
#[repr(transparent)]
pub struct GcRaw<T>(*const GcMem<T>);

impl<T> GcRaw<T> {
   /// Returns a reference to the data inside the `GcRaw<T>`.
   ///
   /// # Safety
   /// The caller must ensure that the `GcRaw<T>` points to existing data.
   pub unsafe fn get<'a>(&self) -> &'a T {
      let mem = &*self.0;
      &mem.data
   }

   pub(crate) fn from_raw(raw: *const GcMem<T>) -> Self {
      Self(raw)
   }

   pub(crate) fn get_raw(&self) -> *const GcMem<T> {
      self.0
   }

   /// Returns a reference to the `GcMem<T>` behind this `GcRaw<T>`.
   ///
   /// # Safety
   /// The caller must ensure that the `GcRaw<T>` points to existing data.
   unsafe fn get_mem(&self) -> &GcMem<T> {
      &*self.0
   }

   /// Marks the reference as still reachable.
   ///
   /// # Safety
   /// The caller must ensure that the `GcRaw<T>` points to existing data.
   unsafe fn mark_reachable(&self) {
      self.get_mem().reachable.set(true);
   }
}

impl<T> Clone for GcRaw<T> {
   fn clone(&self) -> Self {
      Self(self.0)
   }
}

impl<T> Copy for GcRaw<T> {}

impl<T> PartialEq for GcRaw<T> {
   fn eq(&self, other: &Self) -> bool {
      self.0 == other.0
   }
}

impl<T> Eq for GcRaw<T> {}

/// An automatically managed, safe reference to GC memory.
pub struct Gc<T> {
   mem: GcRaw<T>,
}

impl<T> Gc<T> {
   /// Creates a new `Gc` that is not managed by a garbage collector.
   pub fn new(data: T) -> Self {
      let mem = GcMem::allocate(data);
      unsafe {
         let mem = mem.get_mem();
         mem.managed_by_gc.set(false);
         mem.rc.set(1);
      }
      Self { mem }
   }

   /// Constructs a `Gc` handle from a raw pointer to a `GcMem`.
   ///
   /// # Safety
   /// Assumes the pointer passed points to valid memory.
   pub unsafe fn from_raw(raw: GcRaw<T>) -> Self {
      let mem = &*raw.0;
      mem.rc.set(mem.rc.get() + 1);
      Self { mem: raw }
   }

   /// Returns the raw reference to the GC'd memory without affecting the reference count.
   ///
   /// Note that this is an associated function, and must be called like `Gc::as_raw(&gc)`.
   pub fn as_raw(gc: &Self) -> GcRaw<T> {
      gc.mem
   }
}

impl<T> AsRef<T> for Gc<T> {
   fn as_ref(&self) -> &T {
      unsafe { self.mem.get() }
   }
}

impl<T> Borrow<T> for Gc<T> {
   fn borrow(&self) -> &T {
      unsafe { self.mem.get() }
   }
}

impl<T> Deref for Gc<T> {
   type Target = T;

   fn deref(&self) -> &Self::Target {
      unsafe { self.mem.get() }
   }
}

impl<T> Clone for Gc<T> {
   fn clone(&self) -> Gc<T> {
      unsafe { Gc::from_raw(self.mem) }
   }
}

impl<T> Drop for Gc<T> {
   fn drop(&mut self) {
      let mem = unsafe { &*self.mem.0 };
      mem.rc.set(mem.rc.get() - 1);
      if mem.rc.get() == 0 && !mem.managed_by_gc.get() {
         unsafe { GcMem::deallocate(self.mem) }
      }
   }
}

impl<T> fmt::Debug for Gc<T>
where
   T: fmt::Debug,
{
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      unsafe { fmt::Debug::fmt(self.mem.get(), f) }
   }
}

impl<T> fmt::Display for Gc<T>
where
   T: fmt::Display,
{
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      unsafe { fmt::Display::fmt(self.mem.get(), f) }
   }
}

impl<T> PartialEq for Gc<T>
where
   T: PartialEq,
{
   fn eq(&self, other: &Self) -> bool {
      unsafe { self.mem.get() == other.mem.get() }
   }
}

impl<T> Eq for Gc<T> where T: Eq {}

impl<T> PartialOrd for Gc<T>
where
   T: PartialOrd,
{
   fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
      unsafe { self.mem.get().partial_cmp(other.mem.get()) }
   }
}

impl<T> Ord for Gc<T>
where
   T: Ord,
{
   fn cmp(&self, other: &Self) -> std::cmp::Ordering {
      unsafe { self.mem.get().cmp(other.mem.get()) }
   }
}

impl<T> std::hash::Hash for Gc<T>
where
   T: std::hash::Hash,
{
   fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
      unsafe { self.mem.get().hash(state) };
   }
}
