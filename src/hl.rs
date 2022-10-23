//! This module contains the safe, high-level API of Mica.

pub mod builtin_traits;
mod engine;
mod error;
mod fiber;
mod function;
mod stdlib;
mod traits;
mod types;
mod userdata;
mod value;

pub use engine::*;
pub use error::*;
pub use fiber::*;
pub use function::*;
pub use stdlib::*;
pub use traits::*;
pub use types::*;
pub use userdata::*;
pub use value::*;

/// An ID unique to an engine, identifying a global variable.
///
/// Note that these IDs are not portable across different engine instances.
pub use crate::ll::bytecode::GlobalIndex;
/// An ID unique to an engine, identifying a method signature.
///
/// Note that these IDs are not portable across different engine instances.
pub use crate::ll::bytecode::MethodIndex;
/// An **unsafe** value used internally in the VM.
///
/// Does not provide any safety guarantees as to GC'd object lifetimes.
/// You almost always want [`Value`] instead of this.
pub use crate::ll::value::RawValue;
/// The kind of a [`RawValue`].
pub use crate::ll::value::ValueKind as RawValueKind;
pub use crate::{ll as language, ll::gc::Gc};
