//! `impl` block and trait related things.

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::{FunctionIndex, MethodIndex};
use crate::{ll::error::LanguageErrorKind, MethodParameterCount};

/// The index of a trait in an `impl` block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ImplementedTraitIndex(u16);

impl ImplementedTraitIndex {
    pub(crate) fn to_usize(self) -> usize {
        usize::from(self.0)
    }
}

/// The prototype of a struct. This contains a list of functions, from which closures are
/// constructed at runtime to form a dispatch table.
#[derive(Debug, Default)]
pub(crate) struct Prototype {
    /// Map of method IDs to instance methods. This doesn't include trait methods, which have
    /// to be resolved dynamically.
    pub(crate) instance: HashMap<MethodIndex, FunctionIndex>,

    /// List of implemented trait methods. Because an implemented trait in `as` can be any
    /// expression, we have no way of knowing what the method ID is going to be, so we have to map
    /// trait methods by `(name, arity, trait_index)` pairs rather than method ID.
    pub(crate) trait_instance:
        HashMap<(Rc<str>, MethodParameterCount, ImplementedTraitIndex), FunctionIndex>,

    /// Same as `instance`, but lists methods that are added to the type dtable rather than the
    /// instance dtable.
    ///
    /// Even though we don't support `static` methods in traits, the type is kept the same to
    /// reduce code duplication.
    pub(crate) statics: HashMap<MethodIndex, FunctionIndex>,

    /// The total number of traits implemented by this struct.
    pub(crate) implemented_trait_count: u16,
}

impl Prototype {
    pub(crate) fn implement_next_trait(
        &mut self,
    ) -> Result<ImplementedTraitIndex, LanguageErrorKind> {
        let trait_index = self.implemented_trait_count;
        self.implemented_trait_count = self
            .implemented_trait_count
            .checked_add(1)
            .ok_or(LanguageErrorKind::TooManyTraitsInImpl)?;
        Ok(ImplementedTraitIndex(trait_index))
    }
}

/// The prototype of a trait. Contains a list of all method IDs the trait must implement.
#[derive(Debug)]
pub struct TraitPrototype {
    pub name: Rc<str>,
    /// List of method IDs that this trait requires.
    pub required: HashSet<MethodIndex>,
    /// List of `(method_id, function_id)` mappings that make up the dtable of shims for the trait.
    pub shims: Vec<(MethodIndex, FunctionIndex)>,
}
