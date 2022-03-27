mod number;
mod string;

use std::rc::Rc;

use mica_hl::{StandardLibrary, TypeBuilder};

/// Converts a function that takes a `self` parameter to one that takes a `&self` parameter.
fn ref_self1<A, R>(mut f: impl FnMut(A) -> R) -> impl FnMut(&A) -> R
where
   A: Copy,
{
   move |x| f(*x)
}

/// Converts a function that takes a `self` and one arbitrary parameter to one that takes a `&self`
/// and one arbitrary parameter.
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
      string::define(builder)
   }
}

pub fn lib() -> impl StandardLibrary {
   Lib
}
