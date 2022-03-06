//! Error reporting.

pub type LanguageError = mica_language::common::Error;

/// An error.
#[derive(Debug)]
pub enum Error {
   /// An error occured during compilation.
   Compile(LanguageError),
   /// An error occured during runtime.
   Runtime(LanguageError),
   /// The execution engine is in use.
   EngineInUse,
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
      }
   }
}
