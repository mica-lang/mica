//! Error reporting.

use std::{borrow::Cow, fmt};

/// A raw [`ll`][crate::ll] error, with metadata such as stack traces.
pub type LanguageError = crate::ll::error::LanguageError;
/// A raw [`ll`][crate::ll] error kind.
pub type LanguageErrorKind = crate::ll::error::LanguageErrorKind;

/// An error.
#[derive(Debug)]
pub enum Error {
    /// An error occured during compilation.
    Compile(LanguageError),
    /// An error occured during runtime.
    Runtime(LanguageError),
    /// There are too many globals.
    TooManyGlobals,
    /// Too many functions were created.
    TooManyFunctions,
    /// Too many methods with different signatures were declared.
    TooManyMethods,
    /// Too many arguments were passed to a method or a function.
    TooManyArguments,
    /// Too many traits were created.
    TooManyTraits,
    /// A trait method with too many parameters was created.
    TooManyParametersInTraitMethod,
    /// A type mismatch occured.
    TypeMismatch {
        /// The name of the expected type.
        expected: Cow<'static, str>,
        /// The name of the actual type obtained.
        got: Cow<'static, str>,
    },
    /// Incorrect amount of arguments passed to a function.
    ArgumentCount {
        /// The number of arguments that was expected.
        expected: usize,
        /// The actual number of arguments obtained.
        got: usize,
    },
    /// A type mismatch occured in function arguments.
    ArgumentTypeMismatch {
        /// Which argument had a type mismatch.
        index: usize,
        /// The name of the expected type.
        expected: Cow<'static, str>,
        /// The name of the actual type obtained.
        got: Cow<'static, str>,
    },
    /// A value was mutably borrowed twice.
    ReentrantMutableBorrow,
    /// A user-defined error.
    User(Box<dyn std::error::Error>),
}

impl From<LanguageError> for Error {
    fn from(error: LanguageError) -> Self {
        match &error {
            LanguageError::Compile { .. } => Self::Compile(error),
            LanguageError::Runtime { .. } => Self::Runtime(error),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compile(error) | Self::Runtime(error) => error.fmt(f),
            Self::TooManyGlobals => f.write_str("too many globals"),
            Self::TooManyFunctions => f.write_str("too many functions"),
            Self::TooManyMethods => f.write_str("too many methods with different signatures"),
            Self::TooManyArguments => f.write_str("too many arguments passed to a function"),
            Self::TooManyTraits => f.write_str("too many traits"),
            Self::TooManyParametersInTraitMethod => {
                f.write_str("trait method with too many parameters")
            }
            Self::TypeMismatch { expected, got } => {
                write!(f, "type mismatch, expected {expected} but got {got}")
            }
            Self::ArgumentCount { expected, got } => {
                write!(f, "{expected} arguments expected but got {got}")
            }
            Self::ArgumentTypeMismatch { index, expected, got } => {
                write!(
                    f,
                    "type mismatch at argument {}, expected {expected} but got {got}",
                    index + 1
                )
            }
            Self::ReentrantMutableBorrow => write!(f, "method receiver is in use already"),
            Self::User(error) => write!(f, "{error}"),
        }
    }
}

impl std::error::Error for Error {}

/// Extensions for converting [`Result`]s into a Mica FFI-friendly structure.
pub trait MicaResultExt<T, E> {
    /// Maps the error in the result to an [`Error`].
    fn mica(self) -> Result<T, Error>;
}

/// Transparent wrapper that implements [`std::error::Error`] for a user-defined error.
#[repr(transparent)]
struct UserError<T>(T);

impl<T> fmt::Debug for UserError<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl<T> fmt::Display for UserError<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl<T> std::error::Error for UserError<T> where T: fmt::Debug + fmt::Display {}

impl<T, E> MicaResultExt<T, E> for Result<T, E>
where
    E: fmt::Debug + fmt::Display + 'static,
{
    fn mica(self) -> Result<T, Error> {
        self.map_err(|error| Error::User(Box::new(UserError(error))))
    }
}

pub(crate) fn wrap_in_language_error<T, E>(r: Result<T, E>) -> Result<T, LanguageErrorKind>
where
    E: std::error::Error + 'static,
{
    r.map_err(|error| LanguageErrorKind::User(Box::new(error)))
}

/// Extensions for converting [`Result`]s into a `mica-language` FFI-friendly structure.
pub trait MicaLanguageResultExt<T> {
    /// Maps the error in the result to a [`LanguageErrorKind`].
    fn to_language_error(self) -> Result<T, LanguageErrorKind>;
}

impl<T> MicaLanguageResultExt<T> for Result<T, Error> {
    fn to_language_error(self) -> Result<T, LanguageErrorKind> {
        wrap_in_language_error(self)
    }
}
