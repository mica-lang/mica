use std::hint::unreachable_unchecked;
use std::mem;
use std::rc::Rc;

use super::{Closure, Struct, UserData, ValueCommon, ValueKind};

/// A portable implementation of values.
#[derive(Clone)]
pub enum ValueImpl {
   /// Nil denotes the lack of a value.
   Nil,
   /// The false boolean.
   False,
   /// The true boolean.
   True,
   /// A double-precision floating point number.
   Number(f64),
   /// A string.
   String(Rc<str>),
   /// A function.
   Function(Rc<Closure>),
   /// A struct.
   Struct(Rc<Struct>),
   /// Dynamically-typed user data.
   // Box<dyn Trait> is actually a fat pointer, storing both the data and a dispatch table.
   // Hence why we need to wrap it in an additional Rc.
   UserData(Rc<Box<dyn UserData>>),
}

impl ValueCommon for ValueImpl {
   fn new_nil() -> Self {
      Self::Nil
   }

   fn new_boolean(b: bool) -> Self {
      match b {
         true => Self::True,
         false => Self::False,
      }
   }

   fn new_number(n: f64) -> Self {
      Self::Number(n)
   }

   fn new_string(s: Rc<str>) -> Self {
      Self::String(s)
   }

   fn new_function(f: Rc<Closure>) -> Self {
      Self::Function(f)
   }

   fn new_struct(s: Rc<Struct>) -> Self {
      Self::Struct(s)
   }

   fn new_user_data(u: Rc<Box<dyn UserData>>) -> Self {
      Self::UserData(u)
   }

   fn kind(&self) -> ValueKind {
      match self {
         ValueImpl::Nil => ValueKind::Nil,
         ValueImpl::False | ValueImpl::True => ValueKind::Boolean,
         ValueImpl::Number(_) => ValueKind::Number,
         ValueImpl::String(_) => ValueKind::String,
         ValueImpl::Function(_) => ValueKind::Function,
         ValueImpl::Struct(_) => ValueKind::Struct,
         ValueImpl::UserData(_) => ValueKind::UserData,
      }
   }

   unsafe fn get_boolean_unchecked(&self) -> bool {
      match self {
         Self::True => true,
         Self::False => false,
         _ => unreachable_unchecked(),
      }
   }

   unsafe fn get_number_unchecked(&self) -> &f64 {
      if let Self::Number(x) = self {
         x
      } else {
         unreachable_unchecked()
      }
   }

   unsafe fn get_string_unchecked(&self) -> &Rc<str> {
      if let Self::String(s) = self {
         s
      } else {
         unreachable_unchecked()
      }
   }

   unsafe fn get_function_unchecked(&self) -> &Rc<Closure> {
      if let Self::Function(f) = self {
         f
      } else {
         unreachable_unchecked()
      }
   }

   unsafe fn get_struct_unchecked(&self) -> &Rc<Struct> {
      if let Self::Struct(s) = self {
         s
      } else {
         unreachable_unchecked()
      }
   }

   unsafe fn get_user_data_unchecked(&self) -> &Rc<Box<dyn UserData>> {
      if let Self::UserData(u) = self {
         u
      } else {
         unreachable_unchecked()
      }
   }
}

impl PartialEq for ValueImpl {
   fn eq(&self, other: &Self) -> bool {
      match (self, other) {
         (Self::Number(l), Self::Number(r)) => l == r,
         (Self::String(l), Self::String(r)) => l == r,
         (Self::Function(l), Self::Function(r)) => Rc::ptr_eq(l, r),
         (Self::Struct(l), Self::Struct(r)) => Rc::ptr_eq(l, r),
         _ => mem::discriminant(self) == mem::discriminant(other),
      }
   }
}
