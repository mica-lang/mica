mod raw;

use std::{any::Any, borrow::Cow, fmt};

pub use raw::*;

use crate::{
    ll::{
        gc::Gc,
        value::{self, Closure, Dict, List, RawValue, Struct, Trait, UserData},
    },
    Error, Object,
};

/// A GC'd type whose content cannot be safely accessed.
#[doc(hidden)]
#[derive(Debug)]
pub struct Hidden<T>(pub(crate) Gc<T>);

impl<T> Clone for Hidden<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

/// A dynamically typed value.
#[derive(Clone)]
pub enum Value {
    /// The `nil` literal.
    Nil,
    /// The `false` literal.
    False,
    /// The `true` literal.
    ///
    /// Do note that despite booleans using two different enum variants, they have the same type.
    True,
    /// A `Number` value.
    Number(f64),
    /// A GC'd `String`.
    String(Gc<String>),
    /// A function.
    ///
    /// Functions can be called with [`Engine::call`][crate::Engine::call].
    Function(Hidden<Closure>),
    /// An opaque struct.
    Struct(Hidden<Struct>),
    /// An opaque trait.
    Trait(Hidden<Trait>),
    /// A list.
    ///
    /// Lists are opaque to the Rust API and must be converted into a typed `Vec<T>`.
    List(Hidden<List>),
    /// A dict.
    ///
    /// Dicts are opaque to the Rust API, no conversion function currently exists for them.
    Dict(Hidden<Dict>),
    /// Arbitrarily typed user data.
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
            Value::Trait(s) => &s.0.dtable().type_name,
            Value::List(_) => "List",
            Value::Dict(_) => "Dict",
            Value::UserData(u) => &unsafe { u.dtable() }.type_name,
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

/// **NOTE:** You should generally avoid dealing with raw values.
#[doc(hidden)]
impl From<RawValue> for Value {
    fn from(raw: RawValue) -> Self {
        Self::from_raw(raw)
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

/// **NOTE:** You should generally avoid dealing with raw values. This method in particular could
/// cause you a bad time if you feed temporary `Value`s converted into `RawValue`s into the
/// vector.
#[doc(hidden)]
impl From<Vec<RawValue>> for Value {
    fn from(v: Vec<RawValue>) -> Self {
        Value::List(Hidden(Gc::new(List::new(v))))
    }
}

/// **NOTE:** Again, you should avoid dealing with raw values. See comment above
/// (for `From<Vec<RawValue>>`).
#[doc(hidden)]
impl From<Dict> for Value {
    fn from(d: Dict) -> Self {
        Value::Dict(Hidden(Gc::new(d)))
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
    /// Tries to perform the conversion, returning an [`Error`] on failure.
    fn try_from_value(value: &Value) -> Result<Self, Error>;
}

fn type_mismatch(expected: impl Into<Cow<'static, str>>, got: &Value) -> Error {
    Error::TypeMismatch { expected: expected.into(), got: got.type_name().to_string().into() }
}

impl TryFromValue for Value {
    fn try_from_value(value: &Value) -> Result<Self, Error> {
        Ok(value.clone())
    }
}

/// **NOTE:** You should generally avoid dealing with raw values. This implementation is especially
/// unsafe as the resulting RawValue is **unmanaged**, which means it may outlive the original value
/// and cause memory safety issues.
#[doc(hidden)]
impl TryFromValue for RawValue {
    fn try_from_value(value: &Value) -> Result<Self, Error> {
        Ok(value.to_raw_unmanaged())
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
                    Error::TypeMismatch { expected: format!("{} or Nil", expected).into(), got }
                } else {
                    unreachable!()
                }
            })?)),
        }
    }
}

impl<T> TryFromValue for Vec<T>
where
    T: TryFromValue,
{
    fn try_from_value(value: &Value) -> Result<Self, Error> {
        if let Value::List(l) = value {
            let elements = unsafe { l.0.as_slice() };
            let mut result = vec![];
            for &element in elements {
                result.push(T::try_from_value(&Value::from_raw(element))?);
            }
            Ok(result)
        } else {
            Err(type_mismatch("List", value))
        }
    }
}
