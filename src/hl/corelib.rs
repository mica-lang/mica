use std::fmt::Debug;

use crate::{
    ll::value::{Dict, RawValue, Record, Tuple},
    Engine, Error, TypeBuilder,
};

/// Definitions of basic types provided by a core library. This role is usually fulfilled by
/// [corelib][crate::corelib].
///
/// Each function must return the original builder, possibly with functions added into it.
pub trait CoreLibrary: 'static + Debug + Clone {
    /// Defines the `Nil` type using the given type builder.
    fn define_nil(&self, builder: TypeBuilder<()>) -> TypeBuilder<()>;

    /// Defines the `Boolean` type using the given type builder.
    fn define_boolean(&self, builder: TypeBuilder<bool>) -> TypeBuilder<bool>;

    /// Defines the `Number` type using the given type builder.
    fn define_number(&self, builder: TypeBuilder<f64>) -> TypeBuilder<f64>;

    /// Defines the `String` type using the given type builder.
    fn define_string(&self, builder: TypeBuilder<String>) -> TypeBuilder<String>;

    /// Defines the `List` type using the given type builder.
    fn define_list(&self, builder: TypeBuilder<Vec<RawValue>>) -> TypeBuilder<Vec<RawValue>>;

    /// Defines the `Dict` type using the given type builder.
    fn define_dict(&self, builder: TypeBuilder<Dict>) -> TypeBuilder<Dict>;

    /// Defines the `Tuple` type of the given size using the given type builder.
    fn define_tuple(&self, size: usize, builder: TypeBuilder<Tuple>) -> TypeBuilder<Tuple>;

    /// Defines the `Record` type with the given `(field_name, field_index)` mappings using the
    /// given type builder.
    fn define_record<'a>(
        &self,
        fields: impl Iterator<Item = (&'a str, usize)>,
        builder: TypeBuilder<Record>,
    ) -> TypeBuilder<Record>;

    /// Loads additional types into the engine once the fundamental types are built.
    fn load(&self, engine: &mut Engine) -> Result<(), Error>;
}
