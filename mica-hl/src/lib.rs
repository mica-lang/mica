//! Mica is an embeddable scripting language for Rust.
//!
//! This crate exposes a safe, high-level API akin to Rhai.

#![allow(clippy::or_fun_call)]

mod engine;
mod error;
mod fiber;
mod function;
mod stdlib;
mod types;
mod userdata;
mod value;

pub use engine::*;
pub use error::*;
pub use fiber::*;
pub use function::*;
pub use stdlib::*;
pub use types::*;
pub use userdata::*;
pub use value::*;

pub use mica_language as language;
pub use mica_language::gc::Gc;
/// An **unsafe** value used internally in the VM.
///
/// Does not provide any safety guarantees as to GC'd object lifetimes.
/// You almost always want [`Value`] instead of this.
pub use mica_language::value::RawValue;
/// The kind of a [`RawValue`].
pub use mica_language::value::ValueKind as RawValueKind;
