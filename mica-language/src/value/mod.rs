//! Dynamically typed values.
//!
//! This module abstracts platform specifics such as NaN boxing away into a common interface.

mod portable;

use std::any::Any;
use std::borrow::Cow;
use std::cell::{Cell, UnsafeCell};
use std::cmp::Ordering;
use std::marker::PhantomPinned;
use std::ops::Deref;
use std::pin::Pin;
use std::rc::Rc;
use std::{fmt, mem, ptr};

use crate::bytecode::{DispatchTable, Opr24};
use crate::common::ErrorKind;

use portable::ValueImpl;

/// The kind of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueKind {
   Nil,
   Boolean,
   Number,
   String,
   Function,
   Struct,
   UserData,
}

/// Common interface each `ValueImpl` must implement.
trait ValueCommon: Clone + PartialEq {
   fn new_nil() -> Self;
   fn new_boolean(b: bool) -> Self;
   fn new_number(n: f64) -> Self;
   fn new_string(s: Rc<str>) -> Self;
   fn new_function(f: Rc<Closure>) -> Self;
   fn new_struct(s: Rc<Struct>) -> Self;
   fn new_user_data(u: Rc<Box<dyn UserData>>) -> Self;

   fn kind(&self) -> ValueKind;

   unsafe fn get_boolean_unchecked(&self) -> bool;
   // This returns a reference such that mica-hl can use `f64` as a `self` parameter in methods.
   unsafe fn get_number_unchecked(&self) -> &f64;
   unsafe fn get_string_unchecked(&self) -> &Rc<str>;
   unsafe fn get_function_unchecked(&self) -> &Rc<Closure>;
   unsafe fn get_struct_unchecked(&self) -> &Rc<Struct>;
   unsafe fn get_user_data_unchecked(&self) -> &Rc<Box<dyn UserData>>;
}

fn _value_impl_must_implement_value_common() {
   fn check<T>()
   where
      T: ValueCommon,
   {
   }
   check::<ValueImpl>();
}

#[derive(Clone, PartialEq)]
pub struct Value(ValueImpl);

impl Value {
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
         ValueKind::Struct => {
            unsafe { self.0.get_struct_unchecked() }.dtable().type_name.deref().to_owned().into()
         }
         ValueKind::UserData => {
            unsafe { self.0.get_user_data_unchecked() }.dtable().type_name.deref().to_owned().into()
         }
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
   pub unsafe fn get_string_unchecked(&self) -> &Rc<str> {
      self.0.get_string_unchecked()
   }

   /// Returns a function value without performing any checks.
   ///
   /// # Safety
   /// Calling this on a value that isn't known to be a function is undefined behavior.
   pub unsafe fn get_function_unchecked(&self) -> &Rc<Closure> {
      self.0.get_function_unchecked()
   }

   /// Returns a struct value without performing any checks.
   ///
   /// # Safety
   /// Calling this on a value that isn't known to be a struct is undefined behavior.
   pub unsafe fn get_struct_unchecked(&self) -> &Rc<Struct> {
      self.0.get_struct_unchecked()
   }

   /// Returns a user data value without performing any checks.
   ///
   /// # Safety
   /// Calling this on a value that isn't known to be a user data is undefined behavior.
   pub unsafe fn get_user_data_unchecked(&self) -> &Rc<Box<dyn UserData>> {
      self.0.get_user_data_unchecked()
   }

   /// Ensures the value is a `Nil`, returning a type mismatch error if that's not the case.
   pub fn nil(&self) -> Result<(), ErrorKind> {
      if self.0.kind() == ValueKind::Nil {
         Ok(())
      } else {
         Err(self.type_error("Nil"))
      }
   }

   /// Ensures the value is a `Boolean`, returning a type mismatch error if that's not the case.
   pub fn boolean(&self) -> Result<bool, ErrorKind> {
      if self.0.kind() == ValueKind::Boolean {
         Ok(unsafe { self.0.get_boolean_unchecked() })
      } else {
         Err(self.type_error("Boolean"))
      }
   }

   /// Ensures the value is a `Number`, returning a type mismatch error if that's not the case.
   pub fn number(&self) -> Result<f64, ErrorKind> {
      if self.0.kind() == ValueKind::Number {
         Ok(unsafe { *self.0.get_number_unchecked() })
      } else {
         Err(self.type_error("Number"))
      }
   }

   /// Ensures the value is a `String`, returning a type mismatch error if that's not the case.
   pub fn string(&self) -> Result<&Rc<str>, ErrorKind> {
      if self.0.kind() == ValueKind::String {
         Ok(unsafe { self.0.get_string_unchecked() })
      } else {
         Err(self.type_error("String"))
      }
   }

   /// Ensures the value is a `Function`, returning a type mismatch error if that's not the case.
   pub fn function(&self) -> Result<&Rc<Closure>, ErrorKind> {
      if self.0.kind() == ValueKind::Function {
         Ok(unsafe { self.0.get_function_unchecked() })
      } else {
         Err(self.type_error("Function"))
      }
   }

   /// Ensures the value is a `Struct`, returning a type mismatch error if that's not the case.
   pub fn struct_v(&self) -> Result<&Rc<Struct>, ErrorKind> {
      if self.0.kind() == ValueKind::Struct {
         Ok(unsafe { self.0.get_struct_unchecked() })
      } else {
         Err(self.type_error("(any struct)"))
      }
   }

   /// Ensures the value is a `UserData` of the given type `T`, returning a type mismatch error
   /// that's not the case.
   pub fn get_user_data<T>(&self) -> Option<&Rc<Box<dyn UserData>>>
   where
      T: UserData,
   {
      if self.0.kind() == ValueKind::UserData {
         Some(unsafe { self.0.get_user_data_unchecked() })
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
            ValueKind::String => {
               let a = unsafe { self.0.get_string_unchecked() };
               let b = unsafe { other.0.get_string_unchecked() };
               Ok(Some(a.cmp(b)))
            }
            ValueKind::Function => Ok(None),
            ValueKind::Struct => Ok(None),
            ValueKind::UserData => Ok(None),
         }
      }
   }
}

impl Default for Value {
   fn default() -> Self {
      Self(ValueImpl::new_nil())
   }
}

impl From<()> for Value {
   fn from(_: ()) -> Self {
      Self(ValueImpl::new_nil())
   }
}

impl From<bool> for Value {
   fn from(b: bool) -> Self {
      Self(ValueImpl::new_boolean(b))
   }
}

impl From<f64> for Value {
   fn from(x: f64) -> Self {
      Self(ValueImpl::new_number(x))
   }
}

impl From<Rc<str>> for Value {
   fn from(s: Rc<str>) -> Self {
      Self(ValueImpl::new_string(s))
   }
}

impl From<Rc<Closure>> for Value {
   fn from(f: Rc<Closure>) -> Self {
      Self(ValueImpl::new_function(f))
   }
}

impl From<Rc<Struct>> for Value {
   fn from(s: Rc<Struct>) -> Self {
      Self(ValueImpl::new_struct(s))
   }
}

impl From<Rc<Box<dyn UserData>>> for Value {
   fn from(u: Rc<Box<dyn UserData>>) -> Self {
      Self(ValueImpl::new_user_data(u))
   }
}

impl fmt::Debug for Value {
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
            ValueKind::String => write!(f, "{:?}", self.0.get_string_unchecked().deref()),
            ValueKind::Function => write!(f, "<func>"),
            ValueKind::Struct => dtable(f, self.0.get_struct_unchecked().dtable()),
            ValueKind::UserData => dtable(f, self.0.get_user_data_unchecked().dtable()),
         }
      }
   }
}

impl fmt::Display for Value {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      unsafe {
         match self.0.kind() {
            ValueKind::String => write!(f, "{}", self.0.get_string_unchecked()),
            _ => fmt::Debug::fmt(&self, f),
         }
      }
   }
}

/// The innards of a struct.
///
/// Note that both types and actual constructed structs use the same representation. The difference
/// is that types do not contain associated fields (Mica does not have static fields.)
pub struct Struct {
   /// The disptach table of the struct. This may only be set once, and setting it seals the
   /// struct.
   pub(crate) dtable: UnsafeCell<Rc<DispatchTable>>,
   sealed: Cell<bool>,
   fields: UnsafeCell<Vec<Value>>,
}

impl Struct {
   /// Creates a new `Struct` representing a type.
   pub fn new_type(dtable: Rc<DispatchTable>) -> Self {
      Self {
         dtable: UnsafeCell::new(dtable),
         sealed: Cell::new(false),
         fields: UnsafeCell::new(Vec::new()),
      }
   }

   /// Creates a new instance of this struct type.
   pub(crate) unsafe fn new_instance(&self, field_count: usize) -> Self {
      Self {
         dtable: UnsafeCell::new(self.dtable().instance.clone().unwrap_unchecked()),
         sealed: Cell::new(true),
         fields: UnsafeCell::new(std::iter::repeat(Value::from(())).take(field_count).collect()),
      }
   }

   /// Returns a reference to the dispatch table of the struct.
   pub fn dtable(&self) -> &DispatchTable {
      unsafe { self.dtable.get().as_ref().unwrap_unchecked() }
   }

   /// Implements the struct with the given dispatch table.
   pub(crate) fn implement(&self, dtable: Rc<DispatchTable>) -> Result<(), ErrorKind> {
      if self.sealed.get() {
         return Err(ErrorKind::StructAlreadyImplemented);
      }
      unsafe { *self.dtable.get() = dtable }
      self.sealed.set(true);
      Ok(())
   }

   /// Returns the value of a field.
   ///
   /// # Safety
   /// This does not perform any borrow checks or bounds checks.
   pub(crate) unsafe fn get_field(&self, index: usize) -> &Value {
      (&*self.fields.get()).get_unchecked(index)
   }

   /// Sets the value of a field.
   ///
   /// # Safety
   /// This does not perform any borrow checks or bounds checks.
   pub(crate) unsafe fn set_field(&self, index: usize, value: Value) {
      *(&mut *self.fields.get()).get_unchecked_mut(index) = value;
   }
}

pub trait UserData: Any {
   /// Returns the user data's dispatch table.
   fn dtable(&self) -> &DispatchTable;

   /// Converts a reference to `UserData` to `Any`.
   fn as_any(&self) -> &dyn Any;
}

/// An upvalue captured by a closure.
pub struct Upvalue {
   /// A writable pointer to the variable captured by this upvalue.
   pub(crate) ptr: UnsafeCell<ptr::NonNull<Value>>,
   /// Storage for a closed upvalue.
   closed: UnsafeCell<Value>,

   _pinned: PhantomPinned,
}

impl fmt::Debug for Upvalue {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      f.debug_struct("Upvalue")
         .field("ptr", unsafe { self.ptr.get().as_ref() }.unwrap())
         .field("closed", unsafe { self.closed.get().as_ref() }.unwrap())
         .finish_non_exhaustive()
   }
}

impl Upvalue {
   /// Creates a new upvalue pointing to a live variable.
   pub(crate) fn new(var: ptr::NonNull<Value>) -> Pin<Rc<Upvalue>> {
      Rc::pin(Upvalue {
         ptr: UnsafeCell::new(var),
         closed: UnsafeCell::new(Value::from(())),
         _pinned: PhantomPinned,
      })
   }

   /// Closes an upvalue by `mem::take`ing the value behind the `ptr` into the `closed` field, and
   /// updating the `ptr` field to point to the `closed` field's contents.
   ///
   /// # Safety
   /// The caller must ensure there are no mutable references to the variable at the time of
   /// calling this.
   pub(crate) unsafe fn close(&self) {
      let ptr = &mut *self.ptr.get();
      let closed = &mut *self.closed.get();
      let value = mem::take(ptr.as_mut());
      *closed = value;
      *ptr = ptr::NonNull::new(closed).unwrap();
   }

   /// Returns the value pointed to by this upvalue.
   ///
   /// # Safety
   /// The caller must ensure there are no mutable references to the source variable at the time
   /// of calling this.
   pub(crate) unsafe fn get(&self) -> &Value {
      (*self.ptr.get()).as_ref()
   }

   /// Writes to the variable pointed to by this upvalue.
   ///
   /// # Safety
   /// The caller must ensure there are no mutable references to the source variable at the time
   /// of calling this.
   pub(crate) unsafe fn set(&self, value: Value) {
      *(*self.ptr.get()).as_ptr() = value;
   }
}

/// The runtime representation of a function.
#[derive(Debug)]
pub struct Closure {
   pub function_id: Opr24,
   pub captures: Vec<Pin<Rc<Upvalue>>>,
}
