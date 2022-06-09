use mica_language::value::{Dict, RawValue};

use crate::TypeBuilder;

/// Definitions of basic types provided by a standard library.
/// This role is usually fulfilled by the [`mica-std`](https://crates.io/crates/mica-std) crate.
///
/// Each function must return the original builder, possibly with functions added into it.
pub trait StandardLibrary {
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
}
