//! Error reporting.

use std::borrow::Cow;

pub type LanguageError = mica_language::common::Error;
pub type LanguageErrorKind = mica_language::common::ErrorKind;

/// An error.
#[derive(Debug)]
pub enum Error {
   /// An error occured during compilation.
   Compile(LanguageError),
   /// An error occured during runtime.
   Runtime(LanguageError),
   /// The execution engine is in use.
   EngineInUse,
   /// There are too many globals.
   TooManyGlobals,
   /// Too many functions were created.
   TooManyFunctions,
   /// A type mismatch occured.
   TypeMismatch {
      expected: Cow<'static, str>,
      got: Cow<'static, str>,
   },
   /// Incorrect amount of arguments passed to a function.
   ArgumentCount { expected: usize, got: usize },
   /// A type mismatch occured in function arguments.
   ArgumentTypeMismatch {
      index: usize,
      expected: Cow<'static, str>,
      got: Cow<'static, str>,
   },
}

impl From<LanguageError> for Error {
   fn from(error: LanguageError) -> Self {
      match &error {
         LanguageError::Compile { .. } => Self::Compile(error),
         LanguageError::Runtime { .. } => Self::Runtime(error),
      }
   }
}

impl std::fmt::Display for Error {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Self::Compile(error) | Self::Runtime(error) => error.fmt(f),
         Self::EngineInUse => f.write_str("execution engine is in use by another fiber"),
         Self::TooManyGlobals => f.write_str("too many globals"),
         Self::TooManyFunctions => f.write_str("too many functions"),
         Self::TypeMismatch { expected, got } => {
            write!(f, "type mismatch, expected {expected} but got {got}")
         }
         Self::ArgumentCount { expected, got } => {
            write!(f, "{expected} arguments expected but got {got}")
         }
         Self::ArgumentTypeMismatch {
            index,
            expected,
            got,
         } => {
            write!(
               f,
               "type mismatch at argument {index}, expected {expected} but got {got}"
            )
         }
      }
   }
}

impl std::error::Error for Error {}
