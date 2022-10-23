use std::rc::Rc;

use super::MethodIndex;
use crate::{
    ll::{gc::GcRaw, value::Closure},
    Gc,
};

/// A dispatch table containing functions bound to an instance of a value.
#[derive(Debug)]
pub struct DispatchTable {
    /// The pretty name of the type this dispatch table contains functions for.
    pub pretty_name: Rc<str>,
    pub type_name: Rc<str>,
    /// The "child" dispatch table that holds instance methods.
    pub instance: Option<GcRaw<DispatchTable>>,
    /// The functions in this dispatch table.
    methods: Vec<Option<GcRaw<Closure>>>,
}

impl DispatchTable {
    /// Creates a new, empty dispatch table for a type with the given name.
    fn new(pretty_name: impl Into<Rc<str>>, type_name: impl Into<Rc<str>>) -> Self {
        Self {
            pretty_name: pretty_name.into(),
            type_name: type_name.into(),
            instance: None,
            methods: Vec::new(),
        }
    }

    /// Creates a new, empty type dispatch table with the given type name.
    pub fn new_for_type(type_name: impl Into<Rc<str>>) -> Self {
        let type_name = type_name.into();
        Self::new(format!("type {type_name}"), type_name)
    }

    /// Creates a new, empty instance dispatch table with the given type name.
    pub fn new_for_instance(type_name: impl Into<Rc<str>>) -> Self {
        let type_name = type_name.into();
        Self::new(Rc::clone(&type_name), type_name)
    }

    /// Returns a reference to the method at the given index.
    pub fn get_method(&self, index: MethodIndex) -> Option<GcRaw<Closure>> {
        self.methods.get(index.to_usize()).into_iter().flatten().copied().next()
    }

    /// Adds a method into the dispatch table.
    pub fn set_method(&mut self, index: MethodIndex, closure: GcRaw<Closure>) {
        let index = index.to_usize();
        if index >= self.methods.len() {
            self.methods.resize(index + 1, None);
        }
        self.methods[index] = Some(closure);
    }

    /// Returns an iterator over all methods in this dispatch table.
    pub(crate) fn methods(&self) -> impl Iterator<Item = GcRaw<Closure>> + '_ {
        self.methods.iter().copied().flatten()
    }
}

/// Dispatch tables for instances of builtin types. These should be constructed by the standard
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
            nil: Gc::new(DispatchTable::new("Nil", "Nil")),
            boolean: Gc::new(DispatchTable::new("Boolean", "Boolean")),
            number: Gc::new(DispatchTable::new("Number", "Boolean")),
            string: Gc::new(DispatchTable::new("String", "String")),
            function: Gc::new(DispatchTable::new("Function", "Function")),
            list: Gc::new(DispatchTable::new("List", "List")),
            dict: Gc::new(DispatchTable::new("Dict", "Dict")),
        }
    }
}
