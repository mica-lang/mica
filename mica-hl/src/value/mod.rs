mod raw;

use std::any::Any;
use std::borrow::Cow;
use std::fmt;

use mica_language::gc::Gc;
use mica_language::value::{self, Closure, Struct, UserData};

use crate::{Error, Object};

pub use raw::*;

/// A GC'd type whose content cannot be safely accessed.
pub struct Hidden<T>(pub(crate) Gc<T>);

impl<T> Clone for Hidden<T> {
   fn clone(&self) -> Self {
      Self(self.0.clone())
   }
}

/// A dynamically typed value.
#[derive(Clone)]
pub enum Value {
   Nil,
   False,
   True,
   Number(f64),
   String(Gc<String>),
   Function(Hidden<Closure>),
   Struct(Hidden<Struct>),
   UserData(Gc<Box<dyn UserData>>),
}

impl Value {
   /// Returns the name of this value's type.
   pub fn type_name(&self) -> &str {
      match self {
         Value::Nil => "Nil",
         Value::False => "False",
         Value::True => "True",
         Value::Number(_) => "Number",
         Value::String(_) => "String",
         Value::Function(_) => "Function",
         // Hopefully this doesn't explode.
         Value::Struct(s) => &unsafe { s.0.dtable() }.type_name,
         Value::UserData(u) => &u.dtable().type_name,
      }
   }

   /// Returns whether the value is falsy.
   pub fn is_falsy(&self) -> bool {
      matches!(self, Self::Nil | Self::False)
   }

   /// Returns whether the value is truthy.
   pub fn is_truthy(&self) -> bool {
      !self.is_falsy()
   }
}

impl fmt::Debug for Value {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      fmt::Debug::fmt(&self.to_raw_unmanaged(), f)
   }
}

impl fmt::Display for Value {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      fmt::Display::fmt(&self.to_raw_unmanaged(), f)
   }
}

/// The unit type translates to `Value::Nil`.
impl From<()> for Value {
   fn from(_: ()) -> Self {
      Self::Nil
   }
}

impl From<bool> for Value {
   fn from(b: bool) -> Self {
      match b {
         true => Self::True,
         false => Self::False,
      }
   }
}

macro_rules! value_from_number {
   ($T:ty $(, $doc:literal)?) => {
      $(#[doc = $doc])?
      impl From<$T> for Value {
         fn from(x: $T) -> Self {
            Value::Number(x as f64)
         }
      }
   };
}

value_from_number!(i8);
value_from_number!(i16);
value_from_number!(i32);
value_from_number!(i64,   "**NOTE:** This is a lossy conversion, as an `f64` cannot represent the entire range of an `i64`.");
value_from_number!(isize, "**NOTE:** This is a lossy conversion, as an `f64` cannot represent the entire range of an `isize`.");

value_from_number!(u8);
value_from_number!(u16);
value_from_number!(u32);
value_from_number!(u64,   "**NOTE:** This is a lossy conversion, as an `f64` cannot represent the entire range of a `u64`.");
value_from_number!(usize, "**NOTE:** This is a lossy conversion, as an `f64` cannot represent the entire range of a `usize`.");

value_from_number!(f32);
value_from_number!(f64);

impl From<char> for Value {
   fn from(c: char) -> Self {
      Self::from(c.to_string())
   }
}

impl From<&str> for Value {
   fn from(s: &str) -> Self {
      Self::String(Gc::new(s.to_string()))
   }
}

impl From<String> for Value {
   fn from(s: String) -> Self {
      Self::String(Gc::new(s))
   }
}

impl From<Gc<String>> for Value {
   fn from(s: Gc<String>) -> Self {
      Self::String(s)
   }
}

impl<T> From<Option<T>> for Value
where
   T: Into<Value>,
{
   fn from(opt: Option<T>) -> Self {
      match opt {
         Some(value) => value.into(),
         None => Value::Nil,
      }
   }
}

impl<T> From<Object<T>> for Value
where
   T: Any,
{
   fn from(o: Object<T>) -> Self {
      let user_data: Box<dyn value::UserData> = Box::new(o);
      Self::UserData(Gc::new(user_data))
   }
}

/// Implemented by types that can be constructed from [`Value`]s.
pub trait TryFromValue
where
   Self: Sized,
{
   fn try_from_value(value: &Value) -> Result<Self, Error>;
}

fn type_mismatch(expected: impl Into<Cow<'static, str>>, got: &Value) -> Error {
   Error::TypeMismatch {
      expected: expected.into(),
      got: got.type_name().to_string().into(),
   }
}

impl TryFromValue for Value {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      Ok(value.clone())
   }
}

impl TryFromValue for () {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      if let Value::Nil = value {
         Ok(())
      } else {
         Err(type_mismatch("Nil", value))
      }
   }
}

impl TryFromValue for bool {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      match value {
         Value::True => Ok(true),
         Value::False => Ok(false),
         _ => Err(type_mismatch("Boolean", value)),
      }
   }
}

macro_rules! try_from_value_numeric {
   ($T:ty) => {
      impl TryFromValue for $T {
         fn try_from_value(value: &Value) -> Result<Self, Error> {
            if let Value::Number(number) = value {
               Ok(*number as $T)
            } else {
               Err(type_mismatch("Number", value))
            }
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

impl TryFromValue for Gc<String> {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      if let Value::String(s) = value {
         Ok(Gc::clone(s))
      } else {
         Err(type_mismatch("String", value))
      }
   }
}

impl TryFromValue for String {
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      <Gc<String>>::try_from_value(value).map(|s| s.to_string())
   }
}

impl<T> TryFromValue for Option<T>
where
   T: TryFromValue,
{
   fn try_from_value(value: &Value) -> Result<Self, Error> {
      match value {
         Value::Nil => Ok(None),
         _ => Ok(Some(T::try_from_value(value).map_err(|error| {
            if let Error::TypeMismatch { expected, got } = error {
               Error::TypeMismatch {
                  expected: format!("{} or Nil", expected).into(),
                  got,
               }
            } else {
               unreachable!()
            }
         })?)),
      }
   }
}
