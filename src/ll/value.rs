//! Dynamically typed values.
//!
//! This module abstracts platform specifics such as NaN boxing away into a common interface.

mod closures;
mod dicts;
mod impls;
mod lists;
mod structs;
mod traits;

use std::{
    any::Any, borrow::Cow, cmp::Ordering, fmt, hash::Hasher, hint::unreachable_unchecked,
    marker::PhantomData, ops::Deref,
};

pub use closures::*;
pub use dicts::*;
use impls::ValueImpl;
pub use lists::*;
pub use structs::*;
pub use traits::*;

use super::bytecode::Library;
use crate::ll::{bytecode::DispatchTable, error::LanguageErrorKind, gc::GcRaw};

/// The kind of a [`RawValue`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Nil,
    Boolean,
    Number,
    String,
    Function,
    Struct,
    Trait,
    UserData,
}

/// Common interface each `ValueImpl` must implement.
trait ValueCommon: Clone + PartialEq {
    fn new_nil() -> Self;
    fn new_boolean(b: bool) -> Self;
    fn new_number(n: f64) -> Self;
    fn new_string(s: GcRaw<String>) -> Self;
    fn new_function(f: GcRaw<Closure>) -> Self;
    fn new_struct(s: GcRaw<Struct>) -> Self;
    fn new_trait(s: GcRaw<Trait>) -> Self;
    fn new_user_data(u: GcRaw<Box<dyn UserData>>) -> Self;

    fn kind(&self) -> ValueKind;

    unsafe fn get_boolean_unchecked(&self) -> bool;
    // This returns a reference such that mica-hl can use `f64` as a `self` parameter in methods.
    unsafe fn get_number_unchecked(&self) -> &f64;
    unsafe fn get_raw_string_unchecked(&self) -> GcRaw<String>;
    unsafe fn get_raw_function_unchecked(&self) -> GcRaw<Closure>;
    unsafe fn get_raw_struct_unchecked(&self) -> GcRaw<Struct>;
    unsafe fn get_raw_trait_unchecked(&self) -> GcRaw<Trait>;
    unsafe fn get_raw_user_data_unchecked(&self) -> GcRaw<Box<dyn UserData>>;
}

fn _check_implementations() {
    fn check_value<T: ValueCommon>() {}
    check_value::<ValueImpl>();
}

/// An **unsafe** value used internally in the VM.
///
/// Does not provide any safety guarantees as to GC'd object lifetimes.
/// You almost always want [`Value`][crate::Value] instead of this.
#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct RawValue(ValueImpl, PhantomData<*const ()>);

impl RawValue {
    /// Returns the kind of value stored.
    pub fn kind(&self) -> ValueKind {
        self.0.kind()
    }

    fn type_name(&self) -> Cow<'static, str> {
        match self.0.kind() {
            ValueKind::Nil => "Nil".into(),
            ValueKind::Boolean => "Boolean".into(),
            ValueKind::Number => "Number".into(),
            ValueKind::String => "String".into(),
            ValueKind::Function => "Function".into(),
            ValueKind::Struct => unsafe { self.0.get_raw_struct_unchecked().get().dtable() }
                .type_name
                .deref()
                .to_owned()
                .into(),
            ValueKind::Trait => unsafe { self.0.get_raw_trait_unchecked().get().dtable() }
                .type_name
                .deref()
                .to_owned()
                .into(),
            ValueKind::UserData => unsafe {
                self.0.get_raw_user_data_unchecked().get().type_name().into()
            },
        }
    }

    fn type_error(&self, expected: &'static str) -> LanguageErrorKind {
        LanguageErrorKind::TypeError { expected: Cow::from(expected), got: self.type_name() }
    }

    /// Returns a boolean value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a boolean is undefined behavior.
    pub unsafe fn get_boolean_unchecked(&self) -> bool {
        self.0.get_boolean_unchecked()
    }

    /// Returns a number value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a number is undefined behavior.
    pub unsafe fn get_number_unchecked(&self) -> &f64 {
        self.0.get_number_unchecked()
    }

    /// Returns a string value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a string is undefined behavior.
    pub unsafe fn get_raw_string_unchecked(&self) -> GcRaw<String> {
        self.0.get_raw_string_unchecked()
    }

    /// Returns a function value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a function is undefined behavior.
    pub unsafe fn get_raw_function_unchecked(&self) -> GcRaw<Closure> {
        self.0.get_raw_function_unchecked()
    }

    /// Returns a struct value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a struct is undefined behavior.
    pub unsafe fn get_raw_struct_unchecked(&self) -> GcRaw<Struct> {
        self.0.get_raw_struct_unchecked()
    }

    /// Returns a trait value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a trait is undefined behavior.
    pub unsafe fn get_raw_trait_unchecked(&self) -> GcRaw<Trait> {
        self.0.get_raw_trait_unchecked()
    }

    /// Returns a user data value without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be a user data is undefined behavior.
    pub unsafe fn get_raw_user_data_unchecked(&self) -> GcRaw<Box<dyn UserData>> {
        self.0.get_raw_user_data_unchecked()
    }

    /// Downcasts a user data value into a reference without performing any checks.
    ///
    /// # Safety
    /// Calling this on a value that isn't known to be user data of type T is undefined behavior.
    pub unsafe fn downcast_user_data_unchecked<T>(&self) -> &T
    where
        T: UserData,
    {
        if let Some(value) = self.get_raw_user_data_unchecked().get().as_any().downcast_ref::<T>() {
            value
        } else {
            unreachable_unchecked()
        }
    }

    /// Ensures the value is a `Nil`, returning a type mismatch error if that's not the case.
    pub fn ensure_nil(&self) -> Result<(), LanguageErrorKind> {
        if self.0.kind() == ValueKind::Nil {
            Ok(())
        } else {
            Err(self.type_error("Nil"))
        }
    }

    /// Ensures the value is a `Boolean`, returning a type mismatch error if that's not the case.
    pub fn ensure_boolean(&self) -> Result<bool, LanguageErrorKind> {
        if self.0.kind() == ValueKind::Boolean {
            Ok(unsafe { self.0.get_boolean_unchecked() })
        } else {
            Err(self.type_error("Boolean"))
        }
    }

    /// Ensures the value is a `Number`, returning a type mismatch error if that's not the case.
    pub fn ensure_number(&self) -> Result<f64, LanguageErrorKind> {
        if self.0.kind() == ValueKind::Number {
            Ok(unsafe { *self.0.get_number_unchecked() })
        } else {
            Err(self.type_error("Number"))
        }
    }

    /// Ensures the value is a `String`, returning a type mismatch error if that's not the case.
    pub fn ensure_raw_string(&self) -> Result<GcRaw<String>, LanguageErrorKind> {
        if self.0.kind() == ValueKind::String {
            Ok(unsafe { self.0.get_raw_string_unchecked() })
        } else {
            Err(self.type_error("String"))
        }
    }

    /// Ensures the value is a `Function`, returning a type mismatch error if that's not the case.
    pub fn ensure_raw_function(&self) -> Result<GcRaw<Closure>, LanguageErrorKind> {
        if self.0.kind() == ValueKind::Function {
            Ok(unsafe { self.0.get_raw_function_unchecked() })
        } else {
            Err(self.type_error("Function"))
        }
    }

    /// Ensures the value is a `Struct`, returning a type mismatch error if that's not the case.
    pub fn ensure_raw_struct(&self) -> Result<GcRaw<Struct>, LanguageErrorKind> {
        if self.0.kind() == ValueKind::Struct {
            Ok(unsafe { self.0.get_raw_struct_unchecked() })
        } else {
            Err(self.type_error("any struct"))
        }
    }

    /// Ensures the value is a `Trait`, returning a type mismatch error if that's not the case.
    pub fn ensure_raw_trait(&self) -> Result<GcRaw<Trait>, LanguageErrorKind> {
        if self.0.kind() == ValueKind::Trait {
            Ok(unsafe { self.0.get_raw_trait_unchecked() })
        } else {
            Err(self.type_error("any trait"))
        }
    }

    /// Ensures the value is a `UserData` of the given type `T`, returning a type mismatch error
    /// that's not the case.
    pub fn get_raw_user_data<T>(&self) -> Option<GcRaw<Box<dyn UserData>>>
    where
        T: UserData,
    {
        if self.0.kind() == ValueKind::UserData {
            Some(unsafe { self.0.get_raw_user_data_unchecked() })
        } else {
            None
        }
    }

    /// Returns whether the value is truthy. All values except `Nil` and `False` are truthy.
    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }

    /// Returns whether the values is falsy. The only falsy values are `Nil` and `False`.
    pub fn is_falsy(&self) -> bool {
        self.0.kind() == ValueKind::Nil
            || (self.0.kind() == ValueKind::Boolean && unsafe { !self.0.get_boolean_unchecked() })
    }

    /// Attempts to partially compare this value with another one.
    ///
    /// Returns an error if the types of the two values are not the same.
    pub fn try_partial_cmp(&self, other: &Self) -> Result<Option<Ordering>, LanguageErrorKind> {
        if self.0.kind() != other.0.kind() {
            Err(LanguageErrorKind::TypeError { expected: self.type_name(), got: other.type_name() })
        } else {
            match self.0.kind() {
                ValueKind::Nil => Ok(Some(Ordering::Equal)),
                ValueKind::Boolean => {
                    let a = unsafe { self.0.get_boolean_unchecked() };
                    let b = unsafe { other.0.get_boolean_unchecked() };
                    Ok(Some(a.cmp(&b)))
                }
                ValueKind::Number => {
                    let a = unsafe { self.0.get_number_unchecked() };
                    let b = unsafe { other.0.get_number_unchecked() };
                    Ok(a.partial_cmp(b))
                }
                ValueKind::String => unsafe {
                    let a = self.0.get_raw_string_unchecked();
                    let b = other.0.get_raw_string_unchecked();
                    Ok(Some(a.get().cmp(b.get())))
                },
                ValueKind::Function => Ok(None),
                ValueKind::Struct => Ok(None),
                ValueKind::Trait => Ok(None),
                ValueKind::UserData => unsafe {
                    let a = self.0.get_raw_user_data_unchecked();
                    let b = other.0.get_raw_user_data_unchecked();
                    a.get().try_partial_cmp(b.get().deref())
                },
            }
        }
    }
}

impl Default for RawValue {
    fn default() -> Self {
        Self(ValueImpl::new_nil(), PhantomData)
    }
}

impl From<()> for RawValue {
    fn from(_: ()) -> Self {
        Self(ValueImpl::new_nil(), PhantomData)
    }
}

impl From<bool> for RawValue {
    fn from(b: bool) -> Self {
        Self(ValueImpl::new_boolean(b), PhantomData)
    }
}

impl From<f64> for RawValue {
    fn from(x: f64) -> Self {
        Self(ValueImpl::new_number(x), PhantomData)
    }
}

impl From<GcRaw<String>> for RawValue {
    fn from(s: GcRaw<String>) -> Self {
        Self(ValueImpl::new_string(s), PhantomData)
    }
}

impl From<GcRaw<Closure>> for RawValue {
    fn from(f: GcRaw<Closure>) -> Self {
        Self(ValueImpl::new_function(f), PhantomData)
    }
}

impl From<GcRaw<Struct>> for RawValue {
    fn from(s: GcRaw<Struct>) -> Self {
        Self(ValueImpl::new_struct(s), PhantomData)
    }
}

impl From<GcRaw<Trait>> for RawValue {
    fn from(t: GcRaw<Trait>) -> Self {
        Self(ValueImpl::new_trait(t), PhantomData)
    }
}

impl From<GcRaw<Box<dyn UserData>>> for RawValue {
    fn from(u: GcRaw<Box<dyn UserData>>) -> Self {
        Self(ValueImpl::new_user_data(u), PhantomData)
    }
}

impl fmt::Debug for RawValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn dtable(f: &mut fmt::Formatter, dtable: &DispatchTable) -> fmt::Result {
            let type_name = &dtable.pretty_name;
            write!(f, "<[{type_name}]>")
        }

        unsafe {
            match self.0.kind() {
                ValueKind::Nil => f.write_str("nil"),
                ValueKind::Boolean => write!(f, "{}", self.0.get_boolean_unchecked()),
                ValueKind::Number => write!(f, "{}", self.0.get_number_unchecked()),
                ValueKind::String => {
                    write!(f, "{:?}", self.0.get_raw_string_unchecked().get().deref())
                }
                ValueKind::Function => {
                    write!(f, "<func {:?}>", self.0.get_raw_function_unchecked().get_raw())
                }
                ValueKind::Struct => dtable(f, self.0.get_raw_struct_unchecked().get().dtable()),
                ValueKind::Trait => dtable(f, self.0.get_raw_trait_unchecked().get().dtable()),
                ValueKind::UserData => self.0.get_raw_user_data_unchecked().get().fmt(f),
            }
        }
    }
}

impl fmt::Display for RawValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.0.kind() {
                ValueKind::String => write!(f, "{}", self.0.get_raw_string_unchecked().get()),
                _ => fmt::Debug::fmt(&self, f),
            }
        }
    }
}

pub trait UserData: Any + fmt::Debug {
    /// Returns a GC reference to the user data's dispatch table.
    ///
    /// This dispatch table can be obtained from the provided environment if needed, or taken from
    /// the object itself.
    ///
    /// # Panics
    ///
    /// If the value requires the environment but it's not provided. This only happens with certain
    /// built-in types that are implemented as user data under the hood.
    fn dtable_gcraw(&self, library: Option<&Library>) -> GcRaw<DispatchTable>;

    /// Returns the user data's dispatch table.
    ///
    /// # Safety
    /// This is basically sugar for `dtable_gcraw().get()`, so all the footguns of [`GcRaw::get`]
    /// apply.
    unsafe fn dtable(&self, library: Option<&Library>) -> &DispatchTable {
        self.dtable_gcraw(library).get()
    }

    /// This is overridden by built-in types that need magical treatment (lists, dicts.)
    fn value_kind(&self) -> ValueKind {
        ValueKind::UserData
    }

    /// Implementation of equality used by the `==` operator.
    ///
    /// We can't use `PartialEq` here since we want the argument to be `&dyn UserData`,
    fn partial_eq(&self, other: &dyn UserData) -> bool;

    /// Comparison function.
    ///
    /// We can't use `PartialOrd` here since the comparison function can fail.
    fn try_partial_cmp(&self, other: &dyn UserData) -> Result<Option<Ordering>, LanguageErrorKind>;

    fn hash(&self, hasher: &mut dyn Hasher);

    fn type_name(&self) -> &str;

    fn visit_references(&self, _visit: &mut dyn FnMut(RawValue)) {}

    fn as_any(&self) -> &dyn Any;
}
