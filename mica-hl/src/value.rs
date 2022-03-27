use std::any::Any;
use std::hint::unreachable_unchecked;
use std::ops::Deref;
use std::rc::Rc;

use mica_language::common::ErrorKind;

use crate::{Error, Object, UnsafeMutGuard, UnsafeRefGuard, UserData, Value};

/// Used for converting types into dynamically typed values.
pub trait IntoValue {
   fn into_value(self) -> Value;
}

impl IntoValue for Value {
   fn into_value(self) -> Value {
      self
   }
}

/// The unit type is represented as `nil` inside the VM.
impl IntoValue for () {
   fn into_value(self) -> Value {
      Value::Nil
   }
}

impl IntoValue for bool {
   fn into_value(self) -> Value {
      Value::from(self)
   }
}

macro_rules! to_value_numeric {
   ($T:ty) => {
      impl IntoValue for $T {
         fn into_value(self) -> Value {
            Value::Number(self as f64)
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

impl IntoValue for &str {
   fn into_value(self) -> Value {
      Value::String(Rc::from(self))
   }
}

impl IntoValue for String {
   fn into_value(self) -> Value {
      Value::String(Rc::from(self))
   }
}

impl IntoValue for Rc<str> {
   fn into_value(self) -> Value {
      Value::String(self)
   }
}

impl<T> IntoValue for Option<T>
where
   T: IntoValue,
{
   fn into_value(self) -> Value {
      self.map(|x| x.into_value()).unwrap_or(Value::Nil)
   }
}

impl<T> IntoValue for Object<T>
where
   T: Any,
{
   fn into_value(self) -> Value {
      Value::UserData(Rc::new(Box::new(self)))
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
   type Guard;

   /// Returns a shared reference to `Self` **without checking that the value is of the correct
   /// type**.
   ///
   /// # Safety
   ///
   /// This assumes the value is of the correct type. Calling this with a value whose type is not
   /// `Self` results in undefined behavior.
   ///
   /// Because GATs aren't stable yet, `Self::Guard` can keep a **raw pointer** to the value
   /// and as such must not outlive the value.
   unsafe fn from_value_self(value: &Value) -> Result<(&Self, Self::Guard), Error>;
}

/// `()` used as `Self` means that the method receiver is `nil`.
impl FromValueSelf for () {
   type Guard = ();

   unsafe fn from_value_self(_: &Value) -> Result<(&Self, Self::Guard), Error> {
      Ok((&(), ()))
   }
}

impl FromValueSelf for bool {
   type Guard = ();

   unsafe fn from_value_self(v: &Value) -> Result<(&Self, Self::Guard), Error> {
      Ok((
         match v {
            Value::False => &false,
            Value::True => &true,
            _ => unreachable_unchecked(),
         },
         (),
      ))
   }
}

impl FromValueSelf for f64 {
   type Guard = ();

   unsafe fn from_value_self(v: &Value) -> Result<(&Self, Self::Guard), Error> {
      Ok((
         match v {
            Value::Number(n) => n,
            _ => unreachable_unchecked(),
         },
         (),
      ))
   }
}

impl FromValueSelf for Rc<str> {
   type Guard = ();

   unsafe fn from_value_self(v: &Value) -> Result<(&Self, Self::Guard), Error> {
      Ok((
         match v {
            Value::String(s) => s,
            _ => unreachable_unchecked(),
         },
         (),
      ))
   }
}

impl<T> FromValueSelf for T
where
   T: UserData,
{
   type Guard = UnsafeRefGuard<T>;

   unsafe fn from_value_self(value: &Value) -> Result<(&Self, Self::Guard), Error> {
      match value {
         Value::UserData(user_data) => {
            // That <dyn Any> is a bit cursed; I wonder why I couldn't just call downcast_ref on
            // the Rc<Box<dyn UserData>>.
            if let Some(object) = <dyn Any>::downcast_ref::<Object<T>>(user_data) {
               object.unsafe_borrow()
            } else {
               unreachable_unchecked()
            }
         }
         _ => unreachable_unchecked(),
      }
   }
}

/// Implemented by all types that can be a `&mut self` parameter in an instance function.
pub trait FromValueSelfMut
where
   Self: Sized,
{
   type Guard;

   /// Returns a mutable reference to `Self` **without checking that the value is of the correct
   /// type**.
   ///
   /// This trait is only available for value types with interior mutability, as this function
   /// receives a shared, rather than mutable, reference.
   ///
   /// # Safety
   ///
   /// This assumes the value is of the correct type. Calling this with a value whose type is not
   /// `Self` results in undefined behavior.
   ///
   /// Because GATs aren't stable yet, `Self::Guard` can keep a **raw pointer** to the value
   /// and as such must not outlive the value.
   unsafe fn from_value_self_mut(value: &Value) -> Result<(&mut Self, Self::Guard), Error>;
}

impl<T> FromValueSelfMut for T
where
   T: UserData,
{
   type Guard = UnsafeMutGuard<T>;

   unsafe fn from_value_self_mut(value: &Value) -> Result<(&mut Self, Self::Guard), Error> {
      match value {
         Value::UserData(user_data) => {
            if let Some(object) = user_data.as_any().downcast_ref::<Object<T>>() {
               object.unsafe_borrow_mut()
            } else {
               unreachable_unchecked()
            }
         }
         _ => unreachable_unchecked(),
      }
   }
}
