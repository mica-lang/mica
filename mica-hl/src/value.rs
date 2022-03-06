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

/// Used for converting dynamically typed values into statically typed ones.
pub trait TryFromValue
where
   Self: Sized,
{
   fn try_from_value(value: Value) -> Result<Self, Error>;
}

fn convert_type_mismatch(error: ErrorKind) -> Error {
   if let ErrorKind::TypeError { expected, got } = error {
      Error::TypeMismatch { expected, got }
   } else {
      unreachable!()
   }
}

impl TryFromValue for Value {
   fn try_from_value(value: Value) -> Result<Self, Error> {
      Ok(value)
   }
}

impl TryFromValue for () {
   fn try_from_value(value: Value) -> Result<Self, Error> {
      value.nil().map_err(convert_type_mismatch)
   }
}

impl TryFromValue for bool {
   fn try_from_value(value: Value) -> Result<Self, Error> {
      value.boolean().map_err(convert_type_mismatch)
   }
}

macro_rules! try_from_value_numeric {
   ($T:ty) => {
      impl TryFromValue for $T {
         fn try_from_value(value: Value) -> Result<Self, Error> {
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

/// Unfortunately this is not available for `&str` because lifetimes would be a pain.
impl TryFromValue for String {
   fn try_from_value(value: Value) -> Result<Self, Error> {
      value.string().map(|s| s.to_owned()).map_err(convert_type_mismatch)
   }
}
