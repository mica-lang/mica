use std::any::Any;
use std::hint::unreachable_unchecked;
use std::ops::Deref;
use std::rc::Rc;

use mica_language::common::ErrorKind;
use mica_language::value;
use mica_language::value::ValueKind;

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
      Value::from(self)
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
            Value::from(self as f64)
         }
      }
   };
}

to_value_numeric!(u8);
to_value_numeric!(u16);
to_value_numeric!(u32);
to_value_numeric!(u64);
to_value_numeric!(usize);

to_value_numeric!(i8);
to_value_numeric!(i16);
to_value_numeric!(i32);
to_value_numeric!(i64);
to_value_numeric!(isize);

to_value_numeric!(f32);
to_value_numeric!(f64);

impl IntoValue for char {
   fn into_value(self) -> Value {
      let s: Rc<str> = Rc::from(self.to_string());
      Value::from(s)
   }
}

impl IntoValue for &str {
   fn into_value(self) -> Value {
      let s: Rc<str> = Rc::from(self);
      Value::from(s)
   }
}

impl IntoValue for String {
   fn into_value(self) -> Value {
      let s: Rc<str> = Rc::from(self);
      Value::from(s)
   }
}

impl IntoValue for Rc<str> {
   fn into_value(self) -> Value {
      Value::from(self)
   }
}

impl<T> IntoValue for Option<T>
where
   T: IntoValue,
{
   fn into_value(self) -> Value {
      self.map(|x| x.into_value()).unwrap_or(Value::from(()))
   }
}

impl<T> IntoValue for Object<T>
where
   T: Any,
{
   fn into_value(self) -> Value {
      let object: Rc<Box<dyn value::UserData>> = Rc::new(Box::new(self));
      Value::from(object)
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
try_from_value_numeric!(usize);

try_from_value_numeric!(i8);
try_from_value_numeric!(i16);
try_from_value_numeric!(i32);
try_from_value_numeric!(i64);
try_from_value_numeric!(isize);

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
      if value.kind() == ValueKind::Nil {
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
         match v.get_boolean_unchecked() {
            // I mean… if it works…??
            true => &true,
            false => &false,
         },
         (),
      ))
   }
}

impl FromValueSelf for f64 {
   type Guard = ();

   unsafe fn from_value_self(v: &Value) -> Result<(&Self, Self::Guard), Error> {
      Ok((v.get_number_unchecked(), ()))
   }
}

impl FromValueSelf for Rc<str> {
   type Guard = ();

   unsafe fn from_value_self(v: &Value) -> Result<(&Self, Self::Guard), Error> {
      Ok((v.get_string_unchecked(), ()))
   }
}

impl<T> FromValueSelf for T
where
   T: UserData,
{
   type Guard = UnsafeRefGuard<T>;

   unsafe fn from_value_self(value: &Value) -> Result<(&Self, Self::Guard), Error> {
      let user_data = value.get_user_data_unchecked();
      if let Some(object) = <dyn Any>::downcast_ref::<Object<T>>(user_data) {
         object.unsafe_borrow()
      } else {
         unreachable_unchecked()
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
      let user_data = value.get_user_data_unchecked();
      if let Some(object) = <dyn Any>::downcast_ref::<Object<T>>(user_data) {
         object.unsafe_borrow_mut()
      } else {
         unreachable_unchecked()
      }
   }
}
