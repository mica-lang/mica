use std::{fmt::Debug, rc::Rc};

use super::MethodIndex;
use crate::ll::{gc::GcRaw, value::Closure};

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
        self.methods
            .get(index.to_usize())
            .into_iter()
            .flatten()
            .copied()
            .next()
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
