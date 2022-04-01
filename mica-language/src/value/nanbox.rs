//! NaN-boxed values. These are much less portable than the enum implementation, but each values
//! takes up half as much space (8 bytes vs 16 bytes).

use std::hint::unreachable_unchecked;

use crate::gc::{GcMem, GcRaw};

use super::{Closure, Struct, UserData, ValueCommon, ValueKind};

fn _size_and_alignment_checks() {
   // If any of these checks fail, your platform cannot use NaN boxing.
   const _: () = {
      assert!(std::mem::size_of::<*const ()>() == 8);
      assert!(std::mem::align_of::<Struct>() >= 8);
      assert!(std::mem::align_of::<Closure>() >= 8);
      assert!(std::mem::align_of::<Box<dyn UserData>>() >= 8);
   };
}

/// The NaN-boxed implementation of values.
#[derive(Clone, Copy)]
pub struct ValueImpl(u64);

impl ValueImpl {
   const QNAN: u64 = 0b1111111111111 << 50;
   const SIGN_BIT: u64 = 1 << 63;

   // We use the sign bit for disambiguating between "enum" values and "object" values.
   // Enum values are ones that only have one instance, ie. `nil`, `true`, `false`.
   // Object values are ones that are stored in `Rc`s.
   const SIGN_ENUM: u64 = 0;
   const SIGN_OBJECT: u64 = 1;

   const PAYLOAD_BITS: u64 = !(Self::SIGN_BIT | Self::QNAN);

   // SIGN_ENUM payload bits.
   // Note that the bits start from 1; that is because processors produce a payload of 0 on invalid
   // operations such as dividing zero by zero.
   const ENUM_NIL: u64 = 1;
   const ENUM_FALSE: u64 = 2;
   const ENUM_TRUE: u64 = 3;

   // SIGN_OBJECT kind bits.
   // We exploit the fact that objects are aligned to 8 bytes to pack the object type into the
   // three least significant bits of the number.
   const OBJECT_STRING: u64 = 0;
   const OBJECT_FUNCTION: u64 = 1;
   const OBJECT_STRUCT: u64 = 2;
   const OBJECT_USER_DATA: u64 = 3;

   /// The set of bits used for the object tag.
   /// The tag is three bits used to determine the type of the object.
   const OBJECT_TAG_BITS: u64 = 0b111;
   /// The set of bits used for the object pointer.
   const OBJECT_POINTER_BITS: u64 = !(Self::SIGN_BIT | Self::QNAN | Self::OBJECT_TAG_BITS);

   const NIL_BITS: u64 = Self::enum_nan_bits(Self::ENUM_NIL);
   const FALSE_BITS: u64 = Self::enum_nan_bits(Self::ENUM_FALSE);
   const TRUE_BITS: u64 = Self::enum_nan_bits(Self::ENUM_TRUE);

   /// Creates a new value from a normal float.
   fn from_float(f: f64) -> Self {
      // This function produces a valid bit pattern so it's safe to call.
      #[allow(clippy::transmute_float_to_int)]
      Self(unsafe { std::mem::transmute(f) })
   }

   /// Returns the bit pattern of a NaN.
   const fn nan_bits(sign: u64, payload: u64) -> u64 {
      assert!(payload < (1 << 50), "NaN payload out of range");
      Self::QNAN | (sign << 63) | payload
   }

   /// Creates a new value from a NaN.
   const fn new_nan(sign: u64, payload: u64) -> Self {
      Self(Self::nan_bits(sign, payload))
   }

   /// Returns the bit pattern of an enum NaN with the given payload.
   const fn enum_nan_bits(payload: u64) -> u64 {
      Self::nan_bits(Self::SIGN_ENUM, payload)
   }

   /// Creates a new object NaN with a type tag from a `GcRaw`.
   unsafe fn new_object_nan<T>(tag: u64, gc: GcRaw<T>) -> Self {
      // This cast is fine because `_size_and_alignment_checks` ensures that the size of
      // a usize == size of u64 (8 bytes).
      let pointer = gc.get_raw() as usize as u64;
      Self::new_nan(Self::SIGN_OBJECT, pointer | tag)
   }

   /// Returns whether this value is a number (non-NaN or NaN with a zero payload).
   fn is_number(&self) -> bool {
      (self.0 & Self::QNAN != Self::QNAN) || (self.0 & Self::PAYLOAD_BITS == 0)
   }

   /// Returns whether the value represents an object.
   fn is_object(&self) -> bool {
      (self.0 & Self::SIGN_BIT) == Self::SIGN_BIT && !self.is_number()
   }

   /// Returns the object tag bits. Assumes the value is an object.
   unsafe fn object_tag(&self) -> u64 {
      self.0 & Self::OBJECT_TAG_BITS
   }

   /// Returns the object pointer. Assumes the value is an object.
   unsafe fn object_pointer<T>(&self) -> *const T {
      (self.0 & Self::OBJECT_POINTER_BITS) as usize as *const T
   }

   // The functions below do not perform any checks on what's inside, they just blindly
   // transmute the value to a different type.

   unsafe fn as_float(&self) -> &f64 {
      std::mem::transmute(&self.0)
   }

   unsafe fn as_gc<T>(&self) -> GcRaw<T> {
      let pointer: *const GcMem<T> = self.object_pointer();
      GcRaw::from_raw(pointer)
   }
}

impl ValueCommon for ValueImpl {
   fn new_nil() -> Self {
      Self(Self::NIL_BITS)
   }

   fn new_boolean(b: bool) -> Self {
      Self(match b {
         true => Self::TRUE_BITS,
         false => Self::FALSE_BITS,
      })
   }

   fn new_number(n: f64) -> Self {
      Self::from_float(n)
   }

   fn new_string(s: GcRaw<String>) -> Self {
      unsafe { Self::new_object_nan(Self::OBJECT_STRING, s) }
   }

   fn new_function(f: GcRaw<Closure>) -> Self {
      unsafe { Self::new_object_nan(Self::OBJECT_FUNCTION, f) }
   }

   fn new_struct(s: GcRaw<Struct>) -> Self {
      unsafe { Self::new_object_nan(Self::OBJECT_STRUCT, s) }
   }

   fn new_user_data(u: GcRaw<Box<dyn UserData>>) -> Self {
      unsafe { Self::new_object_nan(Self::OBJECT_USER_DATA, u) }
   }

   fn kind(&self) -> ValueKind {
      match self {
         _ if self.0 == Self::NIL_BITS => ValueKind::Nil,
         _ if self.0 == Self::TRUE_BITS || self.0 == Self::FALSE_BITS => ValueKind::Boolean,
         _ if self.is_object() => unsafe {
            match self.object_tag() {
               Self::OBJECT_STRING => ValueKind::String,
               Self::OBJECT_FUNCTION => ValueKind::Function,
               Self::OBJECT_STRUCT => ValueKind::Struct,
               Self::OBJECT_USER_DATA => ValueKind::UserData,
               _ => unreachable_unchecked(),
            }
         },
         // Assume every other bit pattern is a valid number.
         _ => ValueKind::Number,
      }
   }

   unsafe fn get_boolean_unchecked(&self) -> bool {
      ((self.0 & Self::PAYLOAD_BITS) - Self::ENUM_FALSE) != 0
   }

   unsafe fn get_number_unchecked(&self) -> &f64 {
      self.as_float()
   }

   unsafe fn get_raw_string_unchecked(&self) -> GcRaw<String> {
      self.as_gc()
   }

   unsafe fn get_raw_function_unchecked(&self) -> GcRaw<Closure> {
      self.as_gc()
   }

   unsafe fn get_raw_struct_unchecked(&self) -> GcRaw<Struct> {
      self.as_gc()
   }

   unsafe fn get_raw_user_data_unchecked(&self) -> GcRaw<Box<dyn UserData>> {
      self.as_gc()
   }
}

impl PartialEq for ValueImpl {
   fn eq(&self, other: &Self) -> bool {
      // NOTE: This must be done correctly for ordinary NaNs, where NaN != NaN.
      if self.is_number() && other.is_number() {
         return *unsafe { self.as_float() } == *unsafe { other.as_float() };
      } else if self.is_object()
         && other.is_object()
         && unsafe { self.object_tag() == other.object_tag() }
      {
         unsafe {
            if self.object_tag() == Self::OBJECT_STRING {
               let a = self.as_gc::<String>().get();
               let b = other.as_gc::<String>().get();
               return a == b;
            }
         }
      }
      self.0 == other.0
   }
}
