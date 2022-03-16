use mica_hl::{StandardLibrary, TypeBuilder};

fn ref_self<T, R>(mut f: impl FnMut(T) -> R) -> impl FnMut(&T) -> R
where
   T: Copy,
{
   move |x| f(*x)
}

struct Lib;

impl StandardLibrary for Lib {
   fn define_nil(&mut self, builder: TypeBuilder<()>) -> TypeBuilder<()> {
      builder
   }

   fn define_boolean(&mut self, builder: TypeBuilder<bool>) -> TypeBuilder<bool> {
      builder
   }

   fn define_number(&mut self, builder: TypeBuilder<f64>) -> TypeBuilder<f64> {
      builder.add_function("sqrt", ref_self(f64::sqrt))
   }

   fn define_string(&mut self, builder: TypeBuilder<str>) -> TypeBuilder<str> {
      builder
   }
}

pub fn lib() -> impl StandardLibrary {
   Lib
}
