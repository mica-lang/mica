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
   // This macro is ultra-flexible, in that it could support any calling convention.
   // Not that it needs to.
   // See the other arm for usage.
   (
      @for $variant:tt;
      $ret:ty;
      $($genericdef:tt : $bound:path),+;
      |$result:tt| $map:expr;
      $($types:tt),*
   ) => {
      /// Implementation for a static number of arguments.
      impl<
         $($genericdef,)+
         Fun,
         $($types),+
      > $crate::ForeignFunction<$crate::ffvariants::$variant<($($types,)+)>> for Fun
      where
         $($genericdef: $bound + 'static,)+
         Fun: FnMut($($types),+) -> $ret + 'static,
         $($types: TryFromValue + 'static,)+
      {
         fn parameter_count(&self) -> Option<u16> {
            const N: u16 = {
               let n = 0;
               $(
                  // To force Rust into expanding $var into a sequence of additions, we create an
                  // unused variable to drive the expansion. Constant evaluation will ensure this is
                  // a constant integer by the time compilation is finished.
                  #[allow(unused, non_snake_case)]
                  let $types = ();
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
               // Similar to `parameter_count`, we use $types as a driver for a sequential counter.
               // This time though we actually use the variables.
               $(
                  #[allow(non_snake_case)]
                  let $types = arguments.get(n).mica_language()?;
                  #[allow(unused)]
                  let n = n + 1;
               )+
               {
                  let $result = self($($types,)+);
                  $map
               }
            })
         }
      }
   };

   ($($types:tt),*) => {
      impl_non_varargs!(
         // First we define the trait that is to be implemented.
         @for Fallible;
         // Then the return type of this calling convention.
         Result<Ret, Err>;
         // Then the generic bounds of the `impl`.
         Ret: $crate::ToValue,
         Err: std::error::Error;
         // Then the "mapper" "function" that maps the function's result into the raw
         // calling convention.
         |result| result
            .map(|value| value.to_value())
            .map_err(|error| LanguageErrorKind::User(Box::new(error)));
         // Lastly we pass on the generic parameters entered into the macro.
         $($types),+
      );
      impl_non_varargs!(
         @for Infallible;
         Ret;
         Ret: $crate::ToValue;
         |value| Ok(value.to_value());
         $($types),+
      );
   };
}

// Rust-analyzer isn't too happy about this macro declaring variables with non-snake_case names.
// In reality it doesn't hurt anything and actually makes code _more_ readable and maintainable
// by reducing the amount of arguments we have to pass to the macro.
impl_non_varargs!(A);
impl_non_varargs!(A, B);
impl_non_varargs!(A, B, C);
impl_non_varargs!(A, B, C, D);
impl_non_varargs!(A, B, C, D, E);
impl_non_varargs!(A, B, C, D, E, F);
impl_non_varargs!(A, B, C, D, E, F, G);
impl_non_varargs!(A, B, C, D, E, F, G, H);

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
