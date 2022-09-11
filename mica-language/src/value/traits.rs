use crate::bytecode::{DispatchTable, Opr24};
use crate::gc::GcRaw;

/// Instance of a trait.
pub struct Trait {
   pub(crate) id: Opr24,
   pub(crate) dtable: GcRaw<DispatchTable>,
}

impl Trait {
   pub fn dtable(&self) -> &DispatchTable {
      // SAFETY: This operation is safe, because the dtable of a trait can never be modified.
      unsafe { self.dtable.get() }
   }
}
