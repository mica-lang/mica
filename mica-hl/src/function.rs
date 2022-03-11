use mica_language::value::Value;

use crate::{Error, LanguageErrorKind, RawForeignFunction, ToValue, TryFromValue};

fn create_rawff(
   f: impl FnMut(&[Value]) -> Result<Value, LanguageErrorKind> + 'static,
) -> RawForeignFunction {
   Box::new(f)
}

/// Arguments passed to a varargs function.
#[repr(transparent)]
pub struct Arguments<'a> {
   inner: &'a [Value],
}

impl<'a> Arguments<'a> {
   fn new(arguments: &'a [Value]) -> Self {
      Self { inner: arguments }
   }

   /// Returns the number of arguments passed to the function.
   pub fn count(&self) -> usize {
      self.inner.len()
   }

   /// Raises an error if there weren't exactly `n` arguments passed to the function.
   pub fn expect(&self, n: usize) -> Result<(), Error> {
      if self.count() != n {
         Err(Error::ArgumentCount {
            expected: n,
            got: self.count(),
         })
      } else {
         Ok(())
      }
   }

   /// Returns the `n`th argument converted into the given type. Returns an error if there aren't
   /// enough arguments.
   pub fn get<T>(&self, n: usize) -> Result<T, Error>
   where
      T: TryFromValue,
   {
      let value = self.inner.get(n).cloned().unwrap_or(Value::Nil);
      T::try_from_value(value).map_err(|error| {
         if let Error::TypeMismatch { expected, got } = error {
            Error::ArgumentTypeMismatch {
               index: n,
               expected,
               got,
            }
         } else {
            error
         }
      })
   }

   /// Returns the raw array of arguments.
   pub fn raw(&self) -> &[Value] {
      self.inner
   }
}

/// A Rust function that can be accessed in Mica.
///
/// To spare you the time needed to decipher the absolutely unreadable macro-generated
/// implementations of this trait, functions with the following signatures are supported:
/// - `fn (`[`Arguments`]`) -> R` where `R`: [`ToValue`]
/// - `fn (`[`Arguments`]`) -> Result<R, E>` where
///   - `R`: [`ToValue`]
///   - `E`: [`std::error::Error`]
///
/// The generic parameter `V` is not used inside the trait. Its only purpose is to allow for
/// multiple overlapping implementations of a trait for the same type. See [`fn_variants`] for more
/// information.
pub trait ForeignFunction<V> {
   /// Returns the number of parameters this function has, or `None` if the function accepts a
   /// variable number of arguments.
   ///
   /// The default implementation returns `None`.
   fn parameter_count(&self) -> Option<u16>;

   /// Converts the function to a `RawForeignFunction`.
   fn to_raw_foreign_function(self) -> RawForeignFunction;
}

/// Variants of `ForeignFunction`.
///
/// This is a bit of a hack around Rust's type system not supporting disjoint generic
/// implementations. Each implementation has a corresponding marker zero variant enum in this
/// module, such that eg. the traits `ForeignFunction<foreign::GenericResult>` and
/// `ForeignFunction<foreign::Infallible>` are different, but can both be matched by using a generic
/// parameter.
pub mod fn_variants {
   pub enum VarargsFallible {}
   pub enum VarargsInfallible {}
}

impl<Ret, Err, F> ForeignFunction<fn_variants::VarargsFallible> for F
where
   Ret: ToValue + 'static,
   Err: std::error::Error + 'static,
   F: FnMut(Arguments) -> Result<Ret, Err> + 'static,
{
   fn parameter_count(&self) -> Option<u16> {
      None
   }

   fn to_raw_foreign_function(mut self) -> RawForeignFunction {
      create_rawff(move |args| {
         self(Arguments::new(args))
            .map(|value| value.to_value())
            .map_err(|error| LanguageErrorKind::User(Box::new(error)))
      })
   }
}

impl<Ret, F> ForeignFunction<fn_variants::VarargsInfallible> for F
where
   Ret: ToValue + 'static,
   F: FnMut(Arguments) -> Ret + 'static,
{
   fn parameter_count(&self) -> Option<u16> {
      None
   }

   fn to_raw_foreign_function(mut self) -> RawForeignFunction {
      create_rawff(move |args| Ok(self(Arguments::new(args)).to_value()))
   }
}
