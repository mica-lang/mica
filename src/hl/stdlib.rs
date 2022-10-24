use crate::{
    ll::value::{Dict, RawValue},
    Engine, Error, TypeBuilder,
};

/// Definitions of basic types provided by a core library. This role is usually fulfilled by
/// [corelib][crate::corelib].
///
/// Each function must return the original builder, possibly with functions added into it.
pub trait CoreLibrary {
    /// Defines the `Nil` type using the given type builder.
    fn define_nil(&mut self, builder: TypeBuilder<()>) -> TypeBuilder<()>;

    /// Defines the `Boolean` type using the given type builder.
    fn define_boolean(&mut self, builder: TypeBuilder<bool>) -> TypeBuilder<bool>;

    /// Defines the `Number` type using the given type builder.
    fn define_number(&mut self, builder: TypeBuilder<f64>) -> TypeBuilder<f64>;

    /// Defines the `String` type using the given type builder.
    fn define_string(&mut self, builder: TypeBuilder<String>) -> TypeBuilder<String>;

    /// Defines the `List` type using the given type builder.
    fn define_list(&mut self, builder: TypeBuilder<Vec<RawValue>>) -> TypeBuilder<Vec<RawValue>>;

    /// Defines the `Dict` type using the given type builder.
    fn define_dict(&mut self, builder: TypeBuilder<Dict>) -> TypeBuilder<Dict>;

    /// Loads additional types into the engine once the fundamental types are built.
    fn load(&mut self, engine: &mut Engine) -> Result<(), Error>;
}
