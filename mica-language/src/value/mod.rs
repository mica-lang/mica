//! Dynamically typed values.
//!
//! This module abstracts platform specifics such as NaN boxing away into a common interface.

mod closures;
mod dicts;
mod impls;
mod lists;
mod structs;

use std::any::Any;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Write;
use std::ops::Deref;

use crate::bytecode::DispatchTable;
use crate::common::ErrorKind;
use crate::gc::GcRaw;

use impls::ValueImpl;

pub use closures::*;
pub use dicts::*;
pub use lists::*;
pub use structs::*;

/// The kind of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueKind {
   Nil,
   Boolean,
   Number,
   String,
   Function,
   Struct,
   List,
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
   fn new_list(s: GcRaw<List>) -> Self;
   fn new_user_data(u: GcRaw<Box<dyn UserData>>) -> Self;

   fn kind(&self) -> ValueKind;

   unsafe fn get_boolean_unchecked(&self) -> bool;
   // This returns a reference such that mica-hl can use `f64` as a `self` parameter in methods.
   unsafe fn get_number_unchecked(&self) -> &f64;
   unsafe fn get_raw_string_unchecked(&self) -> GcRaw<String>;
   unsafe fn get_raw_function_unchecked(&self) -> GcRaw<Closure>;
   unsafe fn get_raw_struct_unchecked(&self) -> GcRaw<Struct>;
   unsafe fn get_raw_list_unchecked(&self) -> GcRaw<List>;
   unsafe fn get_raw_user_data_unchecked(&self) -> GcRaw<Box<dyn UserData>>;
}

fn _check_implementations() {
   fn check_value<T: ValueCommon>() {}
   check_value::<ValueImpl>();
}

/// A dynamically-typed, raw value. Does not provide **any** safety guarantees.
#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct RawValue(ValueImpl);

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
         ValueKind::List => "List".into(),
         ValueKind::Struct => unsafe { self.0.get_raw_struct_unchecked().get().dtable() }
            .type_name
            .deref()
            .to_owned()
            .into(),
         ValueKind::UserData => unsafe { self.0.get_raw_user_data_unchecked().get().dtable() }
            .type_name
            .deref()
            .to_owned()
            .into(),
      }
   }

   fn type_error(&self, expected: &'static str) -> ErrorKind {
      ErrorKind::TypeError {
         expected: Cow::from(expected),
         got: self.type_name(),
      }
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

   /// Returns a list value without performing any checks.
   ///
   /// # Safety
   /// Calling this on a value that isn't known to be a list is undefined behavior.
   pub unsafe fn get_raw_list_unchecked(&self) -> GcRaw<List> {
      self.0.get_raw_list_unchecked()
   }

   /// Returns a user data value without performing any checks.
   ///
   /// # Safety
   /// Calling this on a value that isn't known to be a user data is undefined behavior.
   pub unsafe fn get_raw_user_data_unchecked(&self) -> GcRaw<Box<dyn UserData>> {
      self.0.get_raw_user_data_unchecked()
   }

   /// Ensures the value is a `Nil`, returning a type mismatch error if that's not the case.
   pub fn ensure_nil(&self) -> Result<(), ErrorKind> {
      if self.0.kind() == ValueKind::Nil {
         Ok(())
      } else {
         Err(self.type_error("Nil"))
      }
   }

   /// Ensures the value is a `Boolean`, returning a type mismatch error if that's not the case.
   pub fn ensure_boolean(&self) -> Result<bool, ErrorKind> {
      if self.0.kind() == ValueKind::Boolean {
         Ok(unsafe { self.0.get_boolean_unchecked() })
      } else {
         Err(self.type_error("Boolean"))
      }
   }

   /// Ensures the value is a `Number`, returning a type mismatch error if that's not the case.
   pub fn ensure_number(&self) -> Result<f64, ErrorKind> {
      if self.0.kind() == ValueKind::Number {
         Ok(unsafe { *self.0.get_number_unchecked() })
      } else {
         Err(self.type_error("Number"))
      }
   }

   /// Ensures the value is a `String`, returning a type mismatch error if that's not the case.
   pub fn ensure_raw_string(&self) -> Result<GcRaw<String>, ErrorKind> {
      if self.0.kind() == ValueKind::String {
         Ok(unsafe { self.0.get_raw_string_unchecked() })
      } else {
         Err(self.type_error("String"))
      }
   }

   /// Ensures the value is a `Function`, returning a type mismatch error if that's not the case.
   pub fn ensure_raw_function(&self) -> Result<GcRaw<Closure>, ErrorKind> {
      if self.0.kind() == ValueKind::Function {
         Ok(unsafe { self.0.get_raw_function_unchecked() })
      } else {
         Err(self.type_error("Function"))
      }
   }

   /// Ensures the value is a `Struct`, returning a type mismatch error if that's not the case.
   pub fn ensure_raw_struct(&self) -> Result<GcRaw<Struct>, ErrorKind> {
      if self.0.kind() == ValueKind::Struct {
         Ok(unsafe { self.0.get_raw_struct_unchecked() })
      } else {
         Err(self.type_error("(any struct)"))
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
   pub fn try_partial_cmp(&self, other: &Self) -> Result<Option<Ordering>, ErrorKind> {
      if self.0.kind() != other.0.kind() {
         Err(ErrorKind::TypeError {
            expected: self.type_name(),
            got: other.type_name(),
         })
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
            ValueKind::List => unsafe {
               let a = self.0.get_raw_list_unchecked();
               let b = other.0.get_raw_list_unchecked();
               a.get().try_partial_cmp(b.get())
            },
            ValueKind::UserData => Ok(None),
         }
      }
   }
}

impl Default for RawValue {
   fn default() -> Self {
      Self(ValueImpl::new_nil())
   }
}

impl From<()> for RawValue {
   fn from(_: ()) -> Self {
      Self(ValueImpl::new_nil())
   }
}

impl From<bool> for RawValue {
   fn from(b: bool) -> Self {
      Self(ValueImpl::new_boolean(b))
   }
}

impl From<f64> for RawValue {
   fn from(x: f64) -> Self {
      Self(ValueImpl::new_number(x))
   }
}

impl From<GcRaw<String>> for RawValue {
   fn from(s: GcRaw<String>) -> Self {
      Self(ValueImpl::new_string(s))
   }
}

impl From<GcRaw<Closure>> for RawValue {
   fn from(f: GcRaw<Closure>) -> Self {
      Self(ValueImpl::new_function(f))
   }
}

impl From<GcRaw<Struct>> for RawValue {
   fn from(s: GcRaw<Struct>) -> Self {
      Self(ValueImpl::new_struct(s))
   }
}

impl From<GcRaw<List>> for RawValue {
   fn from(s: GcRaw<List>) -> Self {
      Self(ValueImpl::new_list(s))
   }
}

impl From<GcRaw<Box<dyn UserData>>> for RawValue {
   fn from(u: GcRaw<Box<dyn UserData>>) -> Self {
      Self(ValueImpl::new_user_data(u))
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
            ValueKind::String => write!(f, "{:?}", self.0.get_raw_string_unchecked().get().deref()),
            ValueKind::Function => write!(
               f,
               "<func {:?}>",
               self.0.get_raw_function_unchecked().get_raw()
            ),
            ValueKind::List => {
               f.write_char('[')?;
               let list = self.0.get_raw_list_unchecked();
               let elements = list.get().as_slice();
               for (i, element) in elements.iter().enumerate() {
                  if i != 0 {
                     f.write_str(", ")?;
                  }
                  fmt::Debug::fmt(element, f)?;
               }
               f.write_char(']')?;
               Ok(())
            }
            ValueKind::Struct => dtable(f, self.0.get_raw_struct_unchecked().get().dtable()),
            ValueKind::UserData => dtable(f, self.0.get_raw_user_data_unchecked().get().dtable()),
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

pub trait UserData: Any {
   /// Returns a GC reference to the user data's dispatch table.
   fn dtable_gcraw(&self) -> GcRaw<DispatchTable>;

   /// Returns the user data's dispatch table.
   ///
   /// # Safety
   /// This is basically sugar for `dtable_gcraw().get()`, so all the footguns of [`GcRaw::get`]
   /// apply.
   unsafe fn dtable(&self) -> &DispatchTable {
      self.dtable_gcraw().get()
   }

   /// Converts a reference to `UserData` to `Any`.
   fn as_any(&self) -> &dyn Any;
}
