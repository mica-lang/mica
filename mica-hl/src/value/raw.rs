use std::hint::unreachable_unchecked;

use mica_language::gc::{Gc, Memory};
use mica_language::value::{RawValue, ValueKind};

use crate::{Error, Hidden, Object, UnsafeMutGuard, UnsafeRefGuard, UserData, Value};

impl Value {
   /// Converts a safe value to a raw value. Gives management rights of the value to the given GC.
   pub(crate) fn to_raw(&self, gc: &mut Memory) -> RawValue {
      match self {
         Value::Nil => RawValue::from(()),
         Value::False => RawValue::from(false),
         Value::True => RawValue::from(true),
         Value::Number(x) => RawValue::from(*x),
         Value::String(s) => RawValue::from(gc.manage(s)),
         Value::Function(f) => RawValue::from(gc.manage(&f.0)),
         Value::Struct(s) => RawValue::from(gc.manage(&s.0)),
         Value::List(l) => RawValue::from(gc.manage(&l.0)),
         Value::UserData(u) => RawValue::from(gc.manage(u)),
      }
   }

   /// Converts a safe value to a raw value. Gives management rights of the value to the given GC.
   pub(crate) fn to_raw_unmanaged(&self) -> RawValue {
      match self {
         Value::Nil => RawValue::from(()),
         Value::False => RawValue::from(false),
         Value::True => RawValue::from(true),
         Value::Number(x) => RawValue::from(*x),
         Value::String(s) => RawValue::from(Gc::as_raw(s)),
         Value::Function(f) => RawValue::from(Gc::as_raw(&f.0)),
         Value::Struct(s) => RawValue::from(Gc::as_raw(&s.0)),
         Value::List(l) => RawValue::from(Gc::as_raw(&l.0)),
         Value::UserData(u) => RawValue::from(Gc::as_raw(u)),
      }
   }

   /// Converts a raw value into a safe value.
   pub(crate) fn from_raw(raw: RawValue) -> Self {
      unsafe {
         match raw.kind() {
            ValueKind::Nil => Self::from(()),
            ValueKind::Boolean => Self::from(raw.get_boolean_unchecked()),
            ValueKind::Number => Self::from(*raw.get_number_unchecked()),
            ValueKind::String => Self::from(Gc::from_raw(raw.get_raw_string_unchecked())),
            ValueKind::Function => {
               Self::Function(Hidden(Gc::from_raw(raw.get_raw_function_unchecked())))
            }
            ValueKind::Struct => Self::Struct(Hidden(Gc::from_raw(raw.get_raw_struct_unchecked()))),
            ValueKind::List => Self::List(Hidden(Gc::from_raw(raw.get_raw_list_unchecked()))),
            ValueKind::UserData => Self::UserData(Gc::from_raw(raw.get_raw_user_data_unchecked())),
         }
      }
   }
}

/// Implemented by all types that can be a `&self` parameter in an instance function.
///
/// This should never be implemented manually unless you know what you're doing.
pub trait SelfFromRawValue {
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
   unsafe fn self_from_raw_value(value: &RawValue) -> Result<(&Self, Self::Guard), Error>;
}

/// `()` used as `Self` means that the method receiver is `nil`.
impl SelfFromRawValue for () {
   type Guard = ();

   unsafe fn self_from_raw_value(_: &RawValue) -> Result<(&Self, Self::Guard), Error> {
      Ok((&(), ()))
   }
}

impl SelfFromRawValue for bool {
   type Guard = ();

   unsafe fn self_from_raw_value(v: &RawValue) -> Result<(&Self, Self::Guard), Error> {
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

impl SelfFromRawValue for f64 {
   type Guard = ();

   unsafe fn self_from_raw_value(v: &RawValue) -> Result<(&Self, Self::Guard), Error> {
      Ok((v.get_number_unchecked(), ()))
   }
}

impl SelfFromRawValue for String {
   type Guard = ();

   unsafe fn self_from_raw_value(v: &RawValue) -> Result<(&Self, Self::Guard), Error> {
      Ok((v.get_raw_string_unchecked().get(), ()))
   }
}

impl<T> SelfFromRawValue for T
where
   T: UserData,
{
   type Guard = UnsafeRefGuard<T>;

   unsafe fn self_from_raw_value(value: &RawValue) -> Result<(&Self, Self::Guard), Error> {
      let user_data = value.get_raw_user_data_unchecked().get();
      if let Some(object) = user_data.as_any().downcast_ref::<Object<T>>() {
         object.unsafe_borrow()
      } else {
         unreachable_unchecked()
      }
   }
}

/// Implemented by all types that can be a `&mut self` parameter in an instance function.
pub trait MutSelfFromRawValue
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
   unsafe fn mut_self_from_raw_value(value: &RawValue) -> Result<(&mut Self, Self::Guard), Error>;
}

impl<T> MutSelfFromRawValue for T
where
   T: UserData,
{
   type Guard = UnsafeMutGuard<T>;

   unsafe fn mut_self_from_raw_value(value: &RawValue) -> Result<(&mut Self, Self::Guard), Error> {
      let user_data = value.get_raw_user_data_unchecked().get();
      if let Some(object) = user_data.as_any().downcast_ref::<Object<T>>() {
         object.unsafe_borrow_mut()
      } else {
         unreachable_unchecked()
      }
   }
}
