mod number;

use std::rc::Rc;

use mica_hl::{StandardLibrary, TypeBuilder};

fn ref_self1<A, R>(mut f: impl FnMut(A) -> R) -> impl FnMut(&A) -> R
where
   A: Copy,
{
   move |x| f(*x)
}

fn ref_self2<A, B, R>(mut f: impl FnMut(A, B) -> R) -> impl FnMut(&A, B) -> R
where
   A: Copy,
{
   move |x, y| f(*x, y)
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
      number::define(builder)
   }

   fn define_string(&mut self, builder: TypeBuilder<Rc<str>>) -> TypeBuilder<Rc<str>> {
      builder.add_function("cat", |s: &Rc<str>, t: Rc<str>| format!("{}{}", s, t))
   }
}

pub fn lib() -> impl StandardLibrary {
   Lib
}
