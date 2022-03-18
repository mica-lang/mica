use std::hint::unreachable_unchecked;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use mica_language::common::ErrorKind;

use crate::{Error, Value};

/// Used for converting types into dynamically typed values.
pub trait ToValue {
   fn to_value(&self) -> Value;
}

impl ToValue for Value {
   fn to_value(&self) -> Value {
      self.clone()
   }
}

/// The unit type is represented as `nil` inside the VM.
impl ToValue for () {
   fn to_value(&self) -> Value {
      Value::Nil
   }
}

impl ToValue for bool {
   fn to_value(&self) -> Value {
      Value::from(*self)
   }
}

macro_rules! to_value_numeric {
   ($T:ty) => {
      impl ToValue for $T {
         fn to_value(&self) -> Value {
            Value::Number(*self as f64)
         }
      }
   };
}

to_value_numeric!(u8);
to_value_numeric!(u16);
to_value_numeric!(u32);
to_value_numeric!(u64);

to_value_numeric!(i8);
to_value_numeric!(i16);
to_value_numeric!(i32);
to_value_numeric!(i64);

to_value_numeric!(f32);
to_value_numeric!(f64);

impl ToValue for &str {
   fn to_value(&self) -> Value {
      Value::String(Rc::from(*self))
   }
}

impl ToValue for String {
   fn to_value(&self) -> Value {
      Value::String(Rc::from(self.as_str()))
   }
}

impl ToValue for Rc<str> {
   fn to_value(&self) -> Value {
      Value::String(Rc::clone(self))
   }
}

impl<T> ToValue for Option<T>
where
   T: ToValue,
{
   fn to_value(&self) -> Value {
      self.as_ref().map(|x| x.to_value()).unwrap_or(Value::Nil)
   }
}

/// Used for converting dynamically typed values into statically typed ones.
pub trait TryFromValue
where
   Self: Sized,
{
   fn try_from_value(value: &Value) -> Result<Self, Error>;
}

fn convert_type_mismatch(error: ErrorKind) -> Error {
   if let ErrorKind::TypeError { expected, got } = error {
      Error::TypeMismatch { expected, got }
   } else {
      unreachable!()
   }
}

impl TryFromValue for Value {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      Ok(value.clone())
   }
}

impl TryFromValue for () {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      value.nil().map_err(convert_type_mismatch)
   }
}

impl TryFromValue for bool {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      value.boolean().map_err(convert_type_mismatch)
   }
}

macro_rules! try_from_value_numeric {
   ($T:ty) => {
      impl TryFromValue for $T {
         fn try_from_value(value: &Value) -> Result<Self, Error> {
            Ok(value.number().map_err(convert_type_mismatch)? as $T)
         }
      }
   };
}

try_from_value_numeric!(u8);
try_from_value_numeric!(u16);
try_from_value_numeric!(u32);
try_from_value_numeric!(u64);

try_from_value_numeric!(i8);
try_from_value_numeric!(i16);
try_from_value_numeric!(i32);
try_from_value_numeric!(i64);

try_from_value_numeric!(f32);
try_from_value_numeric!(f64);

impl TryFromValue for Rc<str> {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      value.string().cloned().map_err(convert_type_mismatch)
   }
}

impl TryFromValue for String {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      value.string().map(|s| s.deref().to_owned()).map_err(convert_type_mismatch)
   }
}

impl<T> TryFromValue for Option<T>
where
   T: TryFromValue,
{
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      if let Value::Nil = value {
         Ok(None)
      } else {
         Ok(Some(T::try_from_value(value)?))
      }
   }
}

/// Implemented by all types that can be a `&self` parameter in an instance function.
///
/// This should never be implemented manually unless you know what you're doing.
pub trait FromValueSelf {
   /// Returns a shared reference to `Self` **without checking that the value is of the correct
   /// type**.
   ///
   /// # Safety
   ///
   /// This assumes the value is of the correct type. Calling this with a value whose type is not
   /// `Self` results in undefined behavior.
   unsafe fn from_value_self(value: &Value) -> &Self;
}

/// `()` used as `Self` means that the method receiver is `nil`.
impl FromValueSelf for () {
   unsafe fn from_value_self(_: &Value) -> &Self {
      &()
   }
}

impl FromValueSelf for bool {
   unsafe fn from_value_self(v: &Value) -> &Self {
      match v {
         Value::False => &false,
         Value::True => &true,
         _ => unreachable_unchecked(),
      }
   }
}

impl FromValueSelf for f64 {
   unsafe fn from_value_self(v: &Value) -> &Self {
      match v {
         Value::Number(n) => n,
         _ => unreachable_unchecked(),
      }
   }
}

impl FromValueSelf for Rc<str> {
   unsafe fn from_value_self(v: &Value) -> &Self {
      match v {
         Value::String(s) => s,
         _ => unreachable_unchecked(),
      }
   }
}

/// Implemented by all types that can be a `&mut self` parameter in an instance function.
pub trait FromValueSelfMut
where
   Self: Sized,
{
   type Ref: DerefMut<Target = Self>;

   /// Returns a mutable reference to `Self` **without checking that the value is of the correct
   /// type**.
   ///
   /// This trait is only available for value types with interior mutability, as this function
   /// receives a shared, rather than mutable, reference.
   ///
   /// This returns an option because a value may already be mutably borrowed as a result of a
   /// reentrant call into the VM.
   ///
   /// # Safety
   ///
   /// This assumes the value is of the correct type. Calling this with a value whose type is not
   /// `Self` results in undefined behavior.
   unsafe fn from_value_self_mut(value: &Value) -> Result<Self::Ref, Error>;
}
