use std::rc::Rc;

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
      builder
         .add_static("parse", |s: Rc<str>| -> Result<f64, _> { s.parse() })
         .add_function("sqrt", ref_self(f64::sqrt))
   }

   fn define_string(&mut self, builder: TypeBuilder<Rc<str>>) -> TypeBuilder<Rc<str>> {
      builder.add_function("cat", |s: &Rc<str>, t: Rc<str>| format!("{}{}", s, t))
   }
}

pub fn lib() -> impl StandardLibrary {
   Lib
}
