use std::rc::Rc;

use super::Closure;
use crate::ll::{
    bytecode::{DispatchTable, Environment, TraitIndex},
    gc::{GcRaw, Memory},
};

/// Instance of a trait.
#[derive(Debug)]
pub struct Trait {
    pub(crate) id: TraitIndex,
    pub(crate) dtable: GcRaw<DispatchTable>,
}

impl Trait {
    pub fn dtable(&self) -> &DispatchTable {
        // SAFETY: This operation is safe, because the dtable of a trait can never be modified.
        unsafe { self.dtable.get() }
    }
}

/// Creates a new instance of a trait inside the given GC memory.
pub fn create_trait(env: &Environment, gc: &mut Memory, trait_id: TraitIndex) -> GcRaw<Trait> {
    let prototype = env
        .get_trait(trait_id)
        .expect("trait with given ID does not exist");
    let name = &prototype.name;
    let mut dispatch_table = DispatchTable::new_for_type(Rc::clone(name));
    dispatch_table.pretty_name = Rc::from(format!("trait {name}"));
    for &(method_id, function_id) in &prototype.shims {
        let function = unsafe { env.get_function_unchecked(function_id) };
        let closure = gc.allocate(Closure {
            name: Rc::clone(&function.name),
            function_id,
            captures: vec![],
        });
        dispatch_table.set_method(method_id, closure);
    }
    let dispatch_table = gc.allocate(dispatch_table);
    gc.allocate(Trait {
        id: trait_id,
        dtable: dispatch_table,
    })
}
