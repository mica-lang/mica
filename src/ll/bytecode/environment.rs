//! Static execution environment.

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::{BuiltinDispatchTables, Function, Opr24, Prototype, TraitPrototype};
use crate::ll::error::{ErrorKind, RenderedSignature};

/// The unique index of a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FunctionIndex(Opr24);

impl FunctionIndex {
    pub(crate) fn from_opr24(x: Opr24) -> Self {
        Self(x)
    }

    pub(crate) fn to_opr24(self) -> Opr24 {
        self.0
    }
}

/// The index of a method that corresponds to a signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct MethodIndex(u16);

impl MethodIndex {
    pub(crate) fn from_u16(x: u16) -> MethodIndex {
        Self(x)
    }

    pub(crate) fn to_u16(self) -> u16 {
        self.0
    }

    pub(crate) fn to_usize(self) -> usize {
        usize::from(self.0)
    }
}

/// The signature of a method (its name, argument count, and enclosing trait).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodSignature {
    pub name: Rc<str>,
    /// This arity number does not include the implicit `self` argument.
    pub arity: Option<u16>,
    /// The index of the trait this signature belongs to.
    /// When `None`, the function is free and does not belong to any trait.
    pub trait_id: Option<TraitIndex>,
}

impl MethodSignature {
    /// Creates a new method signature for a method that does not belong to a trait.
    pub fn new(name: impl Into<Rc<str>>, arity: impl Into<Option<u16>>) -> Self {
        Self { name: name.into(), arity: arity.into(), trait_id: None }
    }

    /// Renders this signature into one that can be formatted.
    pub fn render(&self, env: &Environment) -> RenderedSignature {
        RenderedSignature {
            name: Rc::clone(&self.name),
            // Subtract 1 to omit `self`.
            arity: self.arity.map(|x| x - 1),
            trait_name: self
                .trait_id
                .and_then(|index| env.get_trait(index))
                .map(|prototype| Rc::clone(&prototype.name)),
        }
    }
}

/// The index of a global in the global storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GlobalIndex(Opr24);

impl GlobalIndex {
    pub(crate) fn to_usize(self) -> usize {
        usize::from(self.0)
    }

    pub(crate) fn from_opr24(x: Opr24) -> Self {
        Self(x)
    }

    pub(crate) fn to_opr24(self) -> Opr24 {
        self.0
    }
}

/// The unique index of a prototype.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct PrototypeIndex(Opr24);

impl PrototypeIndex {
    pub(crate) fn from_opr24(x: Opr24) -> Self {
        Self(x)
    }

    pub(crate) fn to_opr24(self) -> Opr24 {
        self.0
    }
}

/// The unique index of a trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TraitIndex(Opr24);

impl TraitIndex {
    pub(crate) fn from_opr24(x: Opr24) -> Self {
        Self(x)
    }

    pub(crate) fn to_opr24(self) -> Opr24 {
        self.0
    }
}

/// An environment containing information about declared globals, functions, vtables.
#[derive(Debug)]
pub struct Environment {
    /// Mapping from global names to global slots.
    globals: HashMap<String, GlobalIndex>,
    /// Functions in the environment.
    functions: Vec<Function>,
    /// Mapping from named function signatures to method indices.
    method_indices: HashMap<MethodSignature, MethodIndex>,
    /// Mapping from method indices to function signatures.
    method_signatures: Vec<MethodSignature>,
    /// Dispatch tables for builtin types.
    pub builtin_dtables: BuiltinDispatchTables,
    /// `impl` prototypes.
    prototypes: Vec<Option<Prototype>>,
    /// Trait prototypes.
    traits: Vec<TraitPrototype>,
}

impl Environment {
    /// Creates a new, empty environment.
    pub fn new(builtin_dtables: BuiltinDispatchTables) -> Self {
        Self {
            globals: HashMap::new(),
            functions: Vec::new(),
            method_indices: HashMap::new(),
            method_signatures: Vec::new(),
            builtin_dtables,
            prototypes: Vec::new(),
            traits: Vec::new(),
        }
    }

    /// Tries to create a global. Returns the global slot number, or an error if there are too many
    /// globals.
    pub fn create_global(&mut self, name: &str) -> Result<GlobalIndex, ErrorKind> {
        if self.globals.contains_key(name) {
            Ok(*self.globals.get(name).unwrap())
        } else {
            let slot =
                Opr24::try_from(self.globals.len()).map_err(|_| ErrorKind::TooManyGlobals)?;
            let slot = GlobalIndex(slot);
            self.globals.insert(name.to_owned(), slot);
            Ok(slot)
        }
    }

    /// Tries to look up a global. Returns `None` if the global doesn't exist.
    pub fn get_global(&self, name: &str) -> Option<GlobalIndex> {
        self.globals.get(name).copied()
    }

    /// Creates a function and returns its ID.
    pub fn create_function(&mut self, function: Function) -> Result<FunctionIndex, ErrorKind> {
        let slot =
            Opr24::try_from(self.functions.len()).map_err(|_| ErrorKind::TooManyFunctions)?;
        let slot = FunctionIndex(slot);
        self.functions.push(function);
        Ok(slot)
    }

    /// Returns the function with the given ID, as returned by `create_function`.
    /// This function is for internal use in the VM and does not perform any checks, thus is marked
    /// `unsafe`.
    pub(crate) unsafe fn get_function_unchecked(&self, id: FunctionIndex) -> &Function {
        let FunctionIndex(id) = id;
        self.functions.get_unchecked(u32::from(id) as usize)
    }

    /// Tries to look up the index of a method, based on a function signature. Creates a new method
    /// index if there isn't one for the given signature. Returns `Err` if there are too many
    /// function signatures in this environment.
    pub fn get_or_create_method_index(
        &mut self,
        signature: &MethodSignature,
    ) -> Result<MethodIndex, ErrorKind> {
        // Don't use `entry` here to avoid cloning the signature.
        if let Some(&index) = self.method_indices.get(signature) {
            Ok(index)
        } else {
            // The number of entries in self.method_indices and self.function_signatures is always
            // equal, so we can use their `len`s interchangably.
            let index =
                u16::try_from(self.method_indices.len()).map_err(|_| ErrorKind::TooManyMethods)?;
            let index = MethodIndex(index);
            self.method_indices.insert(signature.clone(), index);
            self.method_signatures.push(signature.clone());
            Ok(index)
        }
    }

    /// Looks up the index of a method
    pub(crate) fn get_method_index(&self, signature: &MethodSignature) -> Option<MethodIndex> {
        self.method_indices.get(signature).copied()
    }

    /// Returns the signature for the method with the given ID, or `None` if the method index is
    /// invalid.
    pub fn get_method_signature(&self, method_index: MethodIndex) -> Option<&MethodSignature> {
        self.method_signatures.get(usize::from(method_index.0))
    }

    /// Creates a prototype and returns its ID.
    pub(crate) fn create_prototype(
        &mut self,
        proto: Prototype,
    ) -> Result<PrototypeIndex, ErrorKind> {
        let slot = Opr24::try_from(self.prototypes.len()).map_err(|_| ErrorKind::TooManyImpls)?;
        let slot = PrototypeIndex(slot);
        self.prototypes.push(Some(proto));
        Ok(slot)
    }

    /// Takes out the prototype with the given ID, as returned by `create_prototype`.
    /// This function is for internal use in the VM and does not perform any checks, thus is marked
    /// `unsafe`.
    pub(crate) unsafe fn get_prototype_unchecked(&self, id: PrototypeIndex) -> &Prototype {
        let PrototypeIndex(id) = id;
        let proto = self.prototypes.get_unchecked(usize::from(id)).as_ref().unwrap_unchecked();
        proto
    }

    /// Creates a trait and returns its ID. Use `get_trait_mut` afterwards to modify the trait.
    pub fn create_trait(&mut self, name: Rc<str>) -> Result<TraitIndex, ErrorKind> {
        let slot_index = self.traits.len();
        let slot = Opr24::try_from(slot_index).map_err(|_| ErrorKind::TooManyTraits)?;
        let slot = TraitIndex(slot);
        self.traits.push(TraitPrototype { name, required: HashSet::new(), shims: vec![] });
        Ok(slot)
    }

    /// Returns a reference to the trait with the given ID.
    pub fn get_trait(&self, id: TraitIndex) -> Option<&TraitPrototype> {
        let TraitIndex(id) = id;
        self.traits.get(usize::from(id))
    }

    /// Returns a mutable reference to the trait with the given ID.
    pub fn get_trait_mut(&mut self, id: TraitIndex) -> Option<&mut TraitPrototype> {
        let TraitIndex(id) = id;
        self.traits.get_mut(usize::from(id))
    }
}
