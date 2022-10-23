use std::rc::Rc;

use super::Closure;
use crate::{
    bytecode::{DispatchTable, Environment, Opr24},
    gc::{GcRaw, Memory},
};

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

/// Creates a new instance of a trait inside the given GC memory.
pub fn create_trait(env: &Environment, gc: &mut Memory, trait_id: Opr24) -> GcRaw<Trait> {
    let prototype = env.get_trait(trait_id).expect("trait with given ID does not exist");
    let name = &prototype.name;
    let mut dispatch_table = DispatchTable::new_for_type(Rc::clone(name));
    dispatch_table.pretty_name = Rc::from(format!("trait {name}"));
    for &(method_id, function_id) in &prototype.shims {
        let closure = gc.allocate(Closure { function_id, captures: vec![] });
        dispatch_table.set_method(method_id, closure);
    }
    let dispatch_table = gc.allocate(dispatch_table);
    gc.allocate(Trait { id: trait_id, dtable: dispatch_table })
}
