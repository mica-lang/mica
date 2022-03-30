//! Garbage collection.

use std::alloc::{handle_alloc_error, Layout};
use std::cell::Cell;

use crate::value::Value;

/// A garbage collected pool of a specific type of data.
pub(crate) struct Memory {
   strings: Vec<*const GcMem<String>>,
}

impl Memory {
   pub fn new() -> Self {
      Self {
         strings: Vec::new(),
      }
   }

   /// Marks and sweeps unused allocations.
   ///
   /// # Safety
   /// All root pointers yielded by the iterator must be valid.
   pub(crate) unsafe fn collect(&mut self, roots: impl Iterator<Item = Value>) {}

   /// Allocates data in the pool and returns a pointer to that data.
   ///
   /// # Safety
   /// The pointer must not outlive the pool.
   pub(crate) fn allocate_string(&mut self, data: String) -> *const GcMem<String> {
      let gcmem = GcMem::allocate(data);
      self.strings.push(gcmem);
      gcmem
   }
}

impl Default for Memory {
   fn default() -> Self {
      Self::new()
   }
}

/// An allocation with metadata.
#[repr(align(8))]
pub(crate) struct GcMem<T> {
   /// Whether the memory is still reachable.
   /// This is used by the mark phase to determine which memories should (not) be swept.
   reachable: Cell<bool>,
   /// Whether the memory has been collected and is no longer managed by the GC, but rather by
   /// counting foreign references.
   collected: Cell<bool>,
   /// Foreign references to this memory.
   rc: Cell<usize>,
   /// The data.
   data: T,
}

impl<T> GcMem<T> {
   /// Returns the allocation layout of a `GcMem<T>`.
   fn layout() -> Layout {
      Layout::new::<Self>()
   }

   /// Allocates a `GcMem<T>`.
   fn allocate(data: T) -> *const Self {
      let mem = Self {
         reachable: Cell::new(true),
         collected: Cell::new(false),
         rc: Cell::new(0),
         data,
      };
      let layout = Self::layout();
      let allocation = unsafe { std::alloc::alloc(layout) } as *mut Self;
      if allocation.is_null() {
         handle_alloc_error(layout);
      }
      unsafe { *allocation = mem }
      allocation as *const _
   }

   /// Deallocates a `GcMem<T>`.
   ///
   /// # Safety
   /// `mem` must be a pointer returned by [`allocate`][`Self::allocate`].
   unsafe fn deallocate(mem: *const GcMem<T>) {
      // Ugh, that cast from *const to *mut hurts.
      std::alloc::dealloc(mem as *mut u8, Self::layout())
   }
}

pub struct Gc<T> {
   mem: *const GcMem<T>,
}

impl<T> Gc<T> {
   /// Constructs a `Gc` handle from a raw pointer to a `GcMem`.
   ///
   /// # Safety
   /// Assumes the pointer passed points to valid memory.
   unsafe fn from_raw(raw: *const GcMem<T>) -> Self {
      let mem = &*raw;
      mem.rc.set(mem.rc.get() + 1);
      Self { mem: raw }
   }
}

impl<T> Drop for Gc<T> {
   fn drop(&mut self) {
      let mem = unsafe { &*self.mem };
      mem.rc.set(mem.rc.get() - 1);
      if mem.rc.get() == 0 && mem.collected.get() {}
   }
}
