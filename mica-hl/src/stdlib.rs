use crate::TypeBuilder;

/// Definitions of basic types provided by a standard library.
///
/// This role is usually fulfilled by the [`mica-std`](https://crates.io/crates/mica-std) crate.
pub trait StandardLibrary {
   /// Defines the `Nil` type using the given type builder.
   fn define_nil(&mut self, builder: &mut TypeBuilder<()>);

   /// Defines the `Boolean` type using the given type builder.
   fn define_boolean(&mut self, builder: &mut TypeBuilder<bool>);

   /// Defines the `Number` type using the given type builder.
   fn define_number(&mut self, builder: &mut TypeBuilder<f64>);

   /// Defines the `String` type using the given type builder.
   fn define_string(&mut self, builder: &mut TypeBuilder<&str>);
}
