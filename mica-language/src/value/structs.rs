use std::cell::{Cell, UnsafeCell};

use crate::bytecode::DispatchTable;
use crate::common::ErrorKind;
use crate::gc::GcRaw;

use super::RawValue;

/// The innards of a struct.
///
/// Note that both types and actual constructed structs use the same representation. The difference
/// is that types do not contain associated fields (Mica does not have static fields.)
#[repr(align(8))]
pub struct Struct {
   /// The disptach table of the struct. This may only be set once, and setting it seals the
   /// struct.
   pub(crate) dtable: UnsafeCell<GcRaw<DispatchTable>>,
   sealed: Cell<bool>,
   fields: UnsafeCell<Vec<RawValue>>,
}

impl Struct {
   /// Creates a new `Struct` representing a type.
   pub fn new_type(dtable: GcRaw<DispatchTable>) -> Self {
      Self {
         dtable: UnsafeCell::new(dtable),
         sealed: Cell::new(false),
         fields: UnsafeCell::new(Vec::new()),
      }
   }

   /// Creates a new instance of this struct type.
   pub(crate) unsafe fn new_instance(&self, field_count: usize) -> Self {
      Self {
         dtable: UnsafeCell::new(self.dtable().instance.unwrap_unchecked()),
         sealed: Cell::new(true),
         fields: UnsafeCell::new(std::iter::repeat(RawValue::from(())).take(field_count).collect()),
      }
   }

   /// Returns a reference to the dispatch table of the struct.
   ///
   /// # Safety
   ///
   /// Note that the reference's lifetime does not match the struct's. This is because the reference
   /// actually comes from the GC, but `Struct` does not have a lifetime parameter that would
   /// signify that.
   ///
   /// Because the lifetime of the reference is not tracked, this function is unsafe.
   pub unsafe fn dtable<'a>(&self) -> &'a DispatchTable {
      self.dtable.get().as_ref().unwrap_unchecked().get()
   }

   /// Implements the struct with the given dispatch table.
   pub(crate) fn implement(&self, dtable: GcRaw<DispatchTable>) -> Result<(), ErrorKind> {
      if self.sealed.get() {
         return Err(ErrorKind::StructAlreadyImplemented);
      }
      unsafe { *self.dtable.get() = dtable }
      self.sealed.set(true);
      Ok(())
   }

   /// Returns the value of a field.
   ///
   /// # Safety
   /// This does not perform any borrow checks or bounds checks.
   pub(crate) unsafe fn get_field(&self, index: usize) -> RawValue {
      *(*self.fields.get()).get_unchecked(index)
   }

   /// Sets the value of a field.
   ///
   /// # Safety
   /// This does not perform any borrow checks or bounds checks.
   pub(crate) unsafe fn set_field(&self, index: usize, value: RawValue) {
      *(*self.fields.get()).get_unchecked_mut(index) = value;
   }

   pub(crate) unsafe fn fields(&self) -> impl Iterator<Item = RawValue> + '_ {
      (*self.fields.get()).iter().copied()
   }
}
