//! Static function data.

use std::rc::Rc;

use super::Chunk;
use crate::ll::{
    codegen::variables::{LocalIndex, UpvalueIndex},
    common::ErrorKind,
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
pub type ForeignFunction = Box<dyn Fn(&mut Memory, &[RawValue]) -> Result<RawValue, ErrorKind>>;

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

/// A function prototype.
#[derive(Debug)]
pub struct Function {
    pub name: Rc<str>,
    pub parameter_count: Option<u16>,

    pub kind: FunctionKind,

    /// Set to `true` if the function is to be hidden in stack traces.
    ///
    /// This is useful for functions that are implementation details, such as trait function shims.
    pub hidden_in_stack_traces: bool,
}
