use mica_language::value::Value;

use crate::{Error, LanguageErrorKind, RawForeignFunction, ToValue, TryFromValue};

fn create_rawff(
   f: impl FnMut(&[Value]) -> Result<Value, LanguageErrorKind> + 'static,
) -> RawForeignFunction {
   Box::new(f)
}

/// Arguments passed to a varargs function.
///
/// This is a wrapper that does things like type-checking and arity-checking.
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
   pub fn expect_exactly(&self, n: usize) -> Result<(), Error> {
      if self.count() != n {
         Err(Error::ArgumentCount {
            expected: n,
            got: self.count(),
         })
      } else {
         Ok(())
      }
   }

   /// Raises an error if there weren't at least `n` arguments passed to the function.
   pub fn expect_at_least(&self, n: usize) -> Result<(), Error> {
      if self.count() < n {
         Err(Error::ArgumentCount {
            expected: n,
            got: self.count(),
         })
      } else {
         Ok(())
      }
   }

   /// Returns the `n`th argument, or `None` if the argument is not present.
   pub fn nth(&self, n: usize) -> Option<&Value> {
      self.inner.get(n)
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
   pub fn array(&self) -> &[Value] {
      self.inner
   }
}

/// A Rust function that can be called from Mica.
///
/// To spare you the time needed to decipher the absolutely unreadable macro-generated
/// implementations of this trait, functions with the following signatures are supported:
/// - Constant number of type checked arguments
///   - `fn (A, B, C, ...) -> R` where
///     - Each argument: [`TryFromValue`]
///     - `R`: [`ToValue`]
///   - `fn (A, B, C, ...) -> Result<R, E>`
///     - Each argument: [`TryFromValue`]
///     - `R`: [`ToValue`]
///   - Due to a limitation in Rust's type system, a maximum of 8 arguments is supported now.
///     If more is needed, use the varargs versions described below.
/// - Variable number of dynamically typed arguments
///   - `fn (`[`Arguments`]`) -> R` where `R`: [`ToValue`]
///   - `fn (`[`Arguments`]`) -> Result<R, E>` where
///     - `R`: [`ToValue`]
///     - `E`: [`std::error::Error`]
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
pub mod ffvariants {
   use std::marker::PhantomData;

   // For the type system to accept our implemenations, the types of the function's parameters must
   // appear somewhere in the trait or the implemented self type. We can't control the self type,
   // so we put them into a generic parameter here.
   // The PhantomData inside suppresses an "unused generic parameter" error and prevents
   // construction of the two structs (because it's a private field).
   pub struct Fallible<Args>(PhantomData<Args>);
   pub struct Infallible<Args>(PhantomData<Args>);
   pub enum VarargsFallible {}
   pub enum VarargsInfallible {}
}

/// Implementation for zero arguments.
impl<Ret, Err, F> ForeignFunction<ffvariants::Fallible<()>> for F
where
   Ret: ToValue + 'static,
   Err: std::error::Error + 'static,
   F: FnMut() -> Result<Ret, Err> + 'static,
{
   fn parameter_count(&self) -> Option<u16> {
      Some(0)
   }

   fn to_raw_foreign_function(mut self) -> RawForeignFunction {
      create_rawff(move |_| {
         self()
            .map(|value| value.to_value())
            .map_err(|error| LanguageErrorKind::User(Box::new(error)))
      })
   }
}

/// Implementation for zero arguments.
impl<Ret, F> ForeignFunction<ffvariants::Infallible<()>> for F
where
   Ret: ToValue + 'static,
   F: FnMut() -> Ret + 'static,
{
   fn parameter_count(&self) -> Option<u16> {
      Some(0)
   }

   fn to_raw_foreign_function(mut self) -> RawForeignFunction {
      create_rawff(move |_| Ok(self().to_value()))
   }
}

macro_rules! impl_non_varargs {
   (
      @for $variant:tt, $ret:ty;
      $($genericdef:tt : $bound:path),+;
      |$result:tt| $map:expr;
      $($types:tt),*; $($var:tt),*
   ) => {
      /// Implementation for a static number of arguments.
      impl<
         $($genericdef,)+
         Fun,
         $($types),+
      > $crate::ForeignFunction<$crate::ffvariants::$variant<($($types,)+)>> for Fun
      where
         $(
            $genericdef: $bound + 'static,
         )+
         Fun: FnMut($($types),+) -> $ret + 'static,
         $(
            $types: TryFromValue + 'static,
         )+
      {
         fn parameter_count(&self) -> Option<u16> {
            const N: u16 = {
               let n = 0;
               $(
                  // To force Rust into expanding $var into a sequence of additions, we create an
                  // unused variable to drive the expansion. Constant evaluation will ensure this is
                  // a constant integer by the time compilation is finished.
                  #[allow(unused)]
                  let $var = ();
                  let n = n + 1;
               )+
               n
            };
            Some(N)
         }

         fn to_raw_foreign_function(mut self) -> RawForeignFunction {
            use $crate::MicaLanguageResultExt;
            create_rawff(move |args| {
               let arguments = Arguments::new(args);
               let n = 0;
               $(
                  let $var = arguments.get(n).mica_language()?;
                  #[allow(unused)]
                  let n = n + 1;
               )+
               {
                  let $result = self($($var,)+);
                  $map
               }
            })
         }
      }
   };

   ($($types:tt),*; $($var:tt),*) => {
      impl_non_varargs!(
         @for Fallible, Result<Ret, Err>;
         Ret: $crate::ToValue,
         Err: std::error::Error;
         |result| result
            .map(|value| value.to_value())
            .map_err(|error| LanguageErrorKind::User(Box::new(error)));
         $($types),+; $($var),+
      );
      impl_non_varargs!(
         @for Infallible, Ret;
         Ret: $crate::ToValue;
         |value| Ok(value.to_value());
         $($types),+; $($var),+
      );
   };
}

impl_non_varargs!(A; a);
impl_non_varargs!(A, B; a, b);
impl_non_varargs!(A, B, C; a, b, c);
impl_non_varargs!(A, B, C, D; a, b, c, d);
impl_non_varargs!(A, B, C, D, E; a, b, c, d, e);
impl_non_varargs!(A, B, C, D, E, F; a, b, c, d, e, f);
impl_non_varargs!(A, B, C, D, E, F, G; a, b, c, d, e, f, g);
impl_non_varargs!(A, B, C, D, E, F, G, H; a, b, c, d, e, f, g, h);

/// Implementation for a variable number of arguments.
impl<Ret, Err, F> ForeignFunction<ffvariants::VarargsFallible> for F
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

/// Implementation for a variable number of arguments.
impl<Ret, F> ForeignFunction<ffvariants::VarargsInfallible> for F
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
