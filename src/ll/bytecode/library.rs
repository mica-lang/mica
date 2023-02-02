use std::{
    any::{Any, TypeId},
    collections::HashMap,
    rc::Rc,
};

use super::{DispatchTable, Environment, MethodIndex, TraitIndex};
use crate::{
    ll::{codegen::TraitBuilder, error::LanguageErrorKind},
    Gc, MethodParameterCount,
};

/// Aggregate of all dispatch tables and traits available to a VM.
#[derive(Debug)]
pub struct Library {
    /// Built-in dispatch tables. This has to be initialized with proper dtables by the core
    /// library.
    pub builtin_dtables: BuiltinDispatchTables,
    /// Built-in traits. This is initialized by the compiler itself after the environment is
    /// available.
    pub builtin_traits: BuiltinTraits,

    /// Dispatch tables for user types.
    user_dtables: HashMap<TypeId, Gc<DispatchTable>>,
}

impl Library {
    pub fn new(builtin_dtables: BuiltinDispatchTables, builtin_traits: BuiltinTraits) -> Self {
        Self { builtin_dtables, builtin_traits, user_dtables: HashMap::new() }
    }

    /// Adds a dispatch table for a user-defined type.
    pub fn add_user_dtable<T>(&mut self, dtable: Gc<DispatchTable>)
    where
        T: Any,
    {
        let type_id = TypeId::of::<T>();
        self.user_dtables.insert(type_id, dtable);
    }

    /// Returns the user-defined dispatch table, if available.
    pub fn get_user_dtable<T>(&self) -> Option<&Gc<DispatchTable>>
    where
        T: Any,
    {
        self.user_dtables.get(&TypeId::of::<T>())
    }
}

/// Dispatch tables for instances of builtin types. These should be constructed by the core
/// library.
#[derive(Debug)]
pub struct BuiltinDispatchTables {
    pub nil: Gc<DispatchTable>,
    pub boolean: Gc<DispatchTable>,
    pub number: Gc<DispatchTable>,
    pub string: Gc<DispatchTable>,
    pub function: Gc<DispatchTable>,
    pub list: Gc<DispatchTable>,
    pub dict: Gc<DispatchTable>,
}

/// Default dispatch tables for built-in types are empty and do not implement any methods.
impl BuiltinDispatchTables {
    pub fn empty() -> Self {
        Self {
            nil: Gc::new(DispatchTable::new_for_instance("Nil")),
            boolean: Gc::new(DispatchTable::new_for_instance("Boolean")),
            number: Gc::new(DispatchTable::new_for_instance("Number")),
            string: Gc::new(DispatchTable::new_for_instance("String")),
            function: Gc::new(DispatchTable::new_for_instance("Function")),
            list: Gc::new(DispatchTable::new_for_instance("List")),
            dict: Gc::new(DispatchTable::new_for_instance("Dict")),
        }
    }
}

/// IDs of built-in traits and their methods.
#[derive(Debug)]
pub struct BuiltinTraits {
    pub iterator: TraitIndex,
    pub iterator_has_next: MethodIndex,
    pub iterator_next: MethodIndex,
}

impl BuiltinTraits {
    /// Tries to register built-in traits in the given environment.
    fn try_register_in(env: &mut Environment) -> Result<Self, LanguageErrorKind> {
        let mut builder = TraitBuilder::new(env, None, Rc::from("Iterator"))?;
        let iterator_has_next = builder
            .add_method(Rc::from("has_next"), MethodParameterCount::from_count_with_self(1))?;
        let iterator_next =
            builder.add_method(Rc::from("next"), MethodParameterCount::from_count_with_self(1))?;
        let (iterator, _) = builder.build();

        Ok(Self { iterator, iterator_has_next, iterator_next })
    }

    /// Registers built-in traits in the given environment.
    ///
    /// # Panics
    /// If there are too many traits, methods, or functions registered in the environment.
    pub fn register_in(env: &mut Environment) -> Self {
        Self::try_register_in(env)
            .expect("environment should have enough space for built-in traits")
    }
}
