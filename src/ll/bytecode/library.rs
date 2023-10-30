use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
};

use super::{DispatchTable, Environment, MethodIndex, Opr24, Opr24OutOfRange, TraitIndex};
use crate::{
    ll::{codegen::TraitBuilder, error::LanguageErrorKind, gc::Memory},
    Gc, MethodParameterCount,
};

/// Aggregate of all dispatch tables and traits available to a VM.
#[derive(Debug)]
pub struct Library {
    /// Built-in dispatch tables. This has to be initialized with proper dtables by the core
    /// library.
    pub builtin_dtables: BuiltinDispatchTables,

    builtin_dtable_generator: Box<dyn BuiltinDispatchTableGenerator>,

    /// Built-in traits. This is initialized by the compiler itself after the environment is
    /// available.
    pub builtin_traits: BuiltinTraits,

    /// Dispatch tables for user types.
    user_dtables: HashMap<TypeId, Gc<DispatchTable>>,
}

impl Library {
    pub fn new(
        builtin_dtables: BuiltinDispatchTables,
        builtin_dtable_generator: Box<dyn BuiltinDispatchTableGenerator>,
        builtin_traits: BuiltinTraits,
    ) -> Self {
        Self {
            builtin_dtables,
            builtin_dtable_generator,
            builtin_traits,
            user_dtables: HashMap::new(),
        }
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

    /// Generates the dtable for tuple of the given size if it doesn't exist yet.
    pub(crate) fn generate_tuple(&mut self, env: &mut Environment, gc: &mut Memory, size: usize) {
        if size >= self.builtin_dtables.tuples.len() {
            self.builtin_dtables.tuples.resize(size + 1, None);
        }
        self.builtin_dtables.tuples[size] =
            Some(
                self.builtin_dtable_generator
                    .generate_tuple(env, gc, &self.builtin_traits, size),
            );
    }

    /// Returns the record type index of the record with the given identifier, generating it if it's
    /// not yet available.
    ///
    /// The identifier signifies the fields the record contains, and must be a string that joins
    /// each field name with a `+` separator; eg. `x+y+z`. Field names must be sorted
    /// alphabetically. This is not enforced anywhere, but it's the convention the rest of the
    /// crate follows, and it's used for things like enumerating all fields in a record.
    pub(crate) fn get_or_generate_record(
        &mut self,
        env: &mut Environment,
        gc: &mut Memory,
        identifier: &Rc<str>,
    ) -> Result<RecordTypeIndex, Opr24OutOfRange> {
        if let Some(&index) = self.builtin_dtables.records_by_identifier.get(identifier) {
            Ok(index)
        } else {
            let index = RecordTypeIndex(Opr24::try_from(self.builtin_dtables.records.len())?);
            let dtable = self.builtin_dtable_generator.generate_record(
                env,
                gc,
                &self.builtin_traits,
                identifier,
            );
            self.builtin_dtables.records.push(Rc::new(RecordType {
                dtable,
                identifier: Rc::clone(identifier),
                field_count: if identifier.len() > 0 {
                    identifier.chars().filter(|&c| c == '+').count() + 1
                } else {
                    0
                },
                index,
            }));
            self.builtin_dtables
                .records_by_identifier
                .insert(Rc::clone(identifier), index);
            Ok(index)
        }
    }
}

/// Creates a record identifier `x+y+...` from an iterator of field names.
pub fn make_record_identifier<'a>(fields: impl Iterator<Item = &'a str>) -> Rc<str> {
    let mut identifier = fields.fold(String::new(), |mut a, b| {
        a.reserve(b.len() + 1);
        a.push_str(b);
        a.push('+');
        a
    });
    identifier.pop();
    Rc::from(identifier)
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
    pub tuples: Vec<Option<Gc<DispatchTable>>>,
    pub records: Vec<Rc<RecordType>>,
    pub records_by_identifier: HashMap<Rc<str>, RecordTypeIndex>,
}

impl BuiltinDispatchTables {
    /// Gets the record type with the given index.
    pub fn get_record(&self, index: RecordTypeIndex) -> &Rc<RecordType> {
        &self.records[usize::from(index.0)]
    }
}

/// The index of a record type stored inside a [`BuiltinDispatchTables`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RecordTypeIndex(Opr24);

impl RecordTypeIndex {
    pub(crate) fn from_opr24(x: Opr24) -> Self {
        Self(x)
    }

    pub(crate) fn to_opr24(self) -> Opr24 {
        self.0
    }
}

/// Describes a record type - combines its dtable with names of fields.
#[derive(Debug)]
pub struct RecordType {
    pub dtable: Gc<DispatchTable>,
    pub identifier: Rc<str>,
    pub field_count: usize,
    pub index: RecordTypeIndex,
}

/// Generator of builtin dispatch tables that need to be generated on demand, such as tuples.
pub trait BuiltinDispatchTableGenerator: Debug {
    fn generate_tuple(
        &self,
        env: &mut Environment,
        gc: &mut Memory,
        builtin_traits: &BuiltinTraits,
        size: usize,
    ) -> Gc<DispatchTable>;

    fn generate_record(
        &self,
        env: &mut Environment,
        gc: &mut Memory,
        builtin_traits: &BuiltinTraits,
        identifier: &str,
    ) -> Gc<DispatchTable>;
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
        let iterator_has_next = builder.add_method(
            Rc::from("has_next"),
            MethodParameterCount::from_count_with_self(1),
        )?;
        let iterator_next = builder.add_method(
            Rc::from("next"),
            MethodParameterCount::from_count_with_self(1),
        )?;
        let (iterator, _) = builder.build();

        Ok(Self {
            iterator,
            iterator_has_next,
            iterator_next,
        })
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
