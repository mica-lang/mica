use mica_hl::{StandardLibrary, TypeBuilder};

fn ref_self<T, R>(mut f: impl FnMut(T) -> R) -> impl FnMut(&T) -> R
where
   T: Copy,
{
   move |x| f(*x)
}

struct Lib;

impl StandardLibrary for Lib {
   fn define_nil(&mut self, _builder: &mut TypeBuilder<()>) {}

   fn define_boolean(&mut self, _builder: &mut TypeBuilder<bool>) {}

   fn define_number(&mut self, builder: &mut TypeBuilder<f64>) {
      builder.add_function("sqrt", ref_self(f64::sqrt));
   }

   fn define_string(&mut self, _builder: &mut TypeBuilder<&str>) {}
}

pub fn lib() -> impl StandardLibrary {
   Lib
}
