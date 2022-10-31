mod raw;

use std::{any::type_name, borrow::Cow, fmt};

pub use raw::*;

use self::into_value::{DoesNotUseEngine, EngineUse, UsesEngine};
use crate::{
    ll::{
        bytecode::{DispatchTable, Environment},
        gc::Gc,
        value::{self, Closure, Dict, List, RawValue, Struct, Trait},
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
    UserData(Gc<Box<dyn value::UserData>>),
}

impl Value {
    /// Creates a new value from any compatible type.
    ///
    /// Note that this can only ever be used with types that can be created without the help of
    /// an [`Engine`][crate::Engine]; in particular, this function is incapable of creating
    /// user data. See [`Engine::create_value`][crate::Engine::create_value] for more information
    /// and a less limited version.
    ///
    /// # Examples
    /// ```
    /// use mica::Value;
    ///
    /// let value = Value::new(1.0);
    /// ```
    /// As mentioned before, [`Value::new`] cannot be used to create user data values:
    /// ```compile_fail
    /// use mica::Value;
    ///
    /// struct Example;
    /// impl UserData for Example {}
    ///
    /// let value = Value::new(Example);
    /// ```
    pub fn new(from: impl IntoValue<EngineUse = DoesNotUseEngine>) -> Self {
        from.into_value(&())
    }

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

/// Helper types for [`IntoValue`].
#[allow(missing_debug_implementations)]
pub mod into_value {
    use crate::ll::bytecode::Environment;

    /// Marker for whether an implementation of [`IntoValue`][crate::IntoValue] requires an engine.
    pub trait EngineUse {
        #[doc(hidden)]
        type Environment;

        #[doc(hidden)]
        fn change_type(env: &Environment) -> &Self::Environment;
    }

    /// Marker for implementations of [`IntoValue`][crate::IntoValue] that do require an engine.
    pub enum UsesEngine {}

    impl EngineUse for UsesEngine {
        type Environment = Environment;

        fn change_type(env: &Environment) -> &Self::Environment {
            env
        }
    }

    /// Marker for implementations of [`IntoValue`][crate::IntoValue] that can be called without an
    /// engine.
    pub enum DoesNotUseEngine {}

    impl EngineUse for DoesNotUseEngine {
        type Environment = ();

        fn change_type(_: &Environment) -> &Self::Environment {
            &()
        }
    }
}

/// Trait implemented by all types that can be converted into [`Value`]s and may require an engine
/// to do so.
///
/// For performing the conversion, see [`Value::new`] and
/// [`Engine::create_value`][crate::Engine::create_value].
pub trait IntoValue {
    /// Specifies whether this implementation of [`IntoValue`] uses an engine or not.
    type EngineUse: into_value::EngineUse;

    /// Performs the conversion.
    #[doc(hidden)]
    fn into_value(self, env: &<Self::EngineUse as EngineUse>::Environment) -> Value;

    /// Performs the conversion providing an environment, regardless of whether the conversion
    /// requires one or not.
    #[doc(hidden)]
    fn into_value_with_environment(self, env: &Environment) -> Value
    where
        Self: Sized,
    {
        self.into_value(<Self::EngineUse as EngineUse>::change_type(env))
    }
}

impl IntoValue for Value {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        self
    }
}

/// **NOTE:** You should generally avoid dealing with raw values.
#[doc(hidden)]
impl IntoValue for RawValue {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::from_raw(self)
    }
}

/// The unit type translates to `Value::Nil`.
impl IntoValue for () {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::Nil
    }
}

impl IntoValue for bool {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        match self {
            true => Value::True,
            false => Value::False,
        }
    }
}

macro_rules! value_from_number {
    ($T:ty $(, $doc:literal)?) => {
        $(#[doc = $doc])?
        impl IntoValue for $T {
            type EngineUse = DoesNotUseEngine;

            fn into_value(self, _: &()) -> Value {
                Value::Number(self as f64)
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

impl IntoValue for char {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::String(Gc::new(self.to_string()))
    }
}

impl IntoValue for &str {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::String(Gc::new(self.to_string()))
    }
}

impl IntoValue for String {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::String(Gc::new(self))
    }
}

impl IntoValue for Gc<String> {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::String(self)
    }
}

impl<T> IntoValue for Option<T>
where
    T: IntoValue,
{
    type EngineUse = T::EngineUse;

    fn into_value(self, env: &<Self::EngineUse as EngineUse>::Environment) -> Value {
        match self {
            Some(value) => value.into_value(env),
            None => Value::Nil,
        }
    }
}

/// **NOTE:** You should generally avoid dealing with raw values. This method in particular could
/// cause you a bad time if you feed temporary `Value`s converted into `RawValue`s into the
/// vector.
#[doc(hidden)]
impl IntoValue for Vec<RawValue> {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::List(Hidden(Gc::new(List::new(self))))
    }
}

/// **NOTE:** Again, you should avoid dealing with raw values. See comment above
/// (for `From<Vec<RawValue>>`).
#[doc(hidden)]
impl IntoValue for Dict {
    type EngineUse = DoesNotUseEngine;

    fn into_value(self, _: &()) -> Value {
        Value::Dict(Hidden(Gc::new(self)))
    }
}

impl<T> IntoValue for T
where
    T: crate::UserData,
{
    type EngineUse = UsesEngine;

    fn into_value(self, env: &Environment) -> Value {
        let dtable = env.get_user_dtable::<T>().map(Gc::clone).unwrap_or_else(|| {
            let ad_hoc_dtable = DispatchTable::new_for_instance(type_name::<T>());
            Gc::new(ad_hoc_dtable)
        });
        let object = Object::new(Gc::as_raw(&dtable), self);
        Value::UserData(Gc::new(Box::new(object)))
    }
}

/// Implemented by types that can be constructed as owned from [`Value`]s.
pub trait TryFromValue
where
    Self: Sized,
{
    /// Tries to perform the conversion, returning an [`Error`] on failure.
    fn try_from_value(value: &Value, env: &Environment) -> Result<Self, Error>;
}

fn type_mismatch(expected: impl Into<Cow<'static, str>>, got: &Value) -> Error {
    Error::TypeMismatch { expected: expected.into(), got: got.type_name().to_string().into() }
}

impl TryFromValue for Value {
    fn try_from_value(value: &Value, _: &Environment) -> Result<Self, Error> {
        Ok(value.clone())
    }
}

/// **NOTE:** You should generally avoid dealing with raw values. This implementation is especially
/// unsafe as the resulting RawValue is **unmanaged**, which means it may outlive the original value
/// and cause memory safety issues.
#[doc(hidden)]
impl TryFromValue for RawValue {
    fn try_from_value(value: &Value, _: &Environment) -> Result<Self, Error> {
        Ok(value.to_raw_unmanaged())
    }
}

impl TryFromValue for () {
    fn try_from_value(value: &Value, _: &Environment) -> Result<Self, Error> {
        if let Value::Nil = value {
            Ok(())
        } else {
            Err(type_mismatch("Nil", value))
        }
    }
}

impl TryFromValue for bool {
    fn try_from_value(value: &Value, _: &Environment) -> Result<Self, Error> {
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
            fn try_from_value(value: &Value, _: &Environment) -> Result<Self, Error> {
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
    fn try_from_value(value: &Value, _: &Environment) -> Result<Self, Error> {
        if let Value::String(s) = value {
            Ok(Gc::clone(s))
        } else {
            Err(type_mismatch("String", value))
        }
    }
}

impl TryFromValue for String {
    fn try_from_value(value: &Value, env: &Environment) -> Result<Self, Error> {
        <Gc<String>>::try_from_value(value, env).map(|s| s.to_string())
    }
}

impl<T> TryFromValue for Option<T>
where
    T: TryFromValue,
{
    fn try_from_value(value: &Value, env: &Environment) -> Result<Self, Error> {
        match value {
            Value::Nil => Ok(None),
            _ => Ok(Some(T::try_from_value(value, env).map_err(|error| {
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
    fn try_from_value(value: &Value, env: &Environment) -> Result<Self, Error> {
        if let Value::List(l) = value {
            let elements = unsafe { l.0.as_slice() };
            let mut result = vec![];
            for &element in elements {
                result.push(T::try_from_value(&Value::from_raw(element), env)?);
            }
            Ok(result)
        } else {
            Err(type_mismatch("List", value))
        }
    }
}

/// It is possible to accept arbitrary types implementing [`UserData`] as parameters to functions,
/// however some limitations apply:
/// - The [`UserData`] **must** implement [`Clone`].
/// - The [`UserData`] **should** be registered inside the engine the value comes from.
///   - If the user data is *ad hoc* (not registered in the engine,) type errors will be less clear
///     as the full Rust type name will be used.
impl<T> TryFromValue for T
where
    T: crate::UserData + Clone,
{
    fn try_from_value(value: &Value, env: &Environment) -> Result<Self, Error> {
        if let Value::UserData(u) = value {
            if let Some(object) = u.as_any().downcast_ref::<Object<T>>() {
                let (object, _guard) = unsafe { object.unsafe_borrow()? };
                return Ok(object.clone());
            }
        }
        let type_name = if let Some(dtable) = env.get_user_dtable::<T>() {
            dtable.type_name.to_string()
        } else {
            type_name::<T>().to_string()
        };
        Err(type_mismatch(type_name, value))
    }
}
