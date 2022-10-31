//! Static function data.

use std::rc::Rc;

use super::{Chunk, Environment};
use crate::ll::{
    codegen::variables::{LocalIndex, UpvalueIndex},
    error::LanguageErrorKind,
    gc::Memory,
    value::RawValue,
};

/// The kind of an upvalue capture.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaptureKind {
    /// Capture a local from the current scope.
    Local(LocalIndex),
    /// Capture an existing upvalue from the current closure.
    Upvalue(UpvalueIndex),
}

/// The signature of a raw foreign function.
pub type ForeignFunction =
    Box<dyn Fn(&Environment, &mut Memory, &[RawValue]) -> Result<RawValue, LanguageErrorKind>>;

/// The kind of a controlling function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Control {
    GcCollect,
}

/// The kind of the function (bytecode or FFI).
pub enum FunctionKind {
    Bytecode { chunk: Rc<Chunk>, captured_locals: Vec<CaptureKind> },
    Foreign(ForeignFunction),
    Control(Control),
}

impl std::fmt::Debug for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bytecode { chunk, captured_locals } => f
                .debug_struct("Bytecode")
                .field("chunk", chunk)
                .field("captured_locals", captured_locals)
                .finish(),
            Self::Foreign(..) => f.debug_struct("Foreign").finish_non_exhaustive(),
            Self::Control(ctl) => f.debug_tuple("Control").field(ctl).finish(),
        }
    }
}

/// The number of parameters in a bare function.
///
/// Bare functions may have fixed or variable numbers of parameters, with up to 65535 arguments
/// supported per call.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionParameterCount {
    /// Accept only a specific number of arguments.
    // Implementation note: Since this count always comes from the syntactic parameter list,
    // this count never includes `self`, even in methods.
    Fixed(u16),
    /// Accept any amount of arguments.
    Varargs,
}

impl FunctionParameterCount {
    /// Attempts to get a fixed parameter count, returning `None` if the count is varargs.
    pub fn to_fixed(self) -> Option<u16> {
        if let Self::Fixed(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl From<u16> for FunctionParameterCount {
    fn from(count: u16) -> Self {
        Self::Fixed(count)
    }
}

impl std::fmt::Debug for FunctionParameterCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                Self::Fixed(num) => write!(f, "Fixed({num})"),
                Self::Varargs => write!(f, "Varargs"),
            }
        } else {
            match self {
                Self::Fixed(num) => write!(f, "{num}"),
                Self::Varargs => write!(f, "..."),
            }
        }
    }
}

/// The number of parameters in a method.
///
/// Unlike [bare functions][FunctionParameterCount], methods can only have fixed parameter counts
/// and can accept up to 255 arguments.
///
/// Internally, this parameter count includes the implicit `self` parameter.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MethodParameterCount(u8);

/// Converting functions from various sources of parameter counts. This usually doesn't need to be
/// used in application code, unless you're dealing with the `ll` API.
impl MethodParameterCount {
    pub fn from_count_without_self(count: impl TryInto<u8>) -> Result<Self, LanguageErrorKind> {
        count
            .try_into()
            .ok()
            .and_then(|x| x.checked_add(1))
            .map(Self)
            .ok_or(LanguageErrorKind::TooManyParameters)
    }

    pub const fn from_count_with_self(count: u8) -> Self {
        Self(count)
    }

    pub fn from_fixed_function_parameter_count(count: u16) -> Result<Self, LanguageErrorKind> {
        Self::from_count_without_self(
            u8::try_from(count).map_err(|_| LanguageErrorKind::TooManyParameters)?,
        )
    }

    pub const fn to_count_without_self(self) -> u8 {
        self.0 - 1
    }

    /// Converts the type-safe [`MethodParameterCount`] into a raw [`u8`] that includes all
    /// parameters.
    ///
    /// You usually want to use this when interfacing with the VM, which does not differentiate
    /// `self` from other arguments.
    pub const fn to_count_with_self(self) -> u8 {
        self.0
    }
}

impl From<u8> for MethodParameterCount {
    fn from(count: u8) -> Self {
        Self(count)
    }
}

impl std::fmt::Debug for MethodParameterCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A function prototype.
#[derive(Debug)]
pub struct Function {
    pub name: Rc<str>,
    pub parameter_count: FunctionParameterCount,

    pub kind: FunctionKind,

    /// Set to `true` if the function is to be hidden in stack traces.
    ///
    /// This is useful for functions that are implementation details, such as trait function shims.
    pub hidden_in_stack_traces: bool,
}
