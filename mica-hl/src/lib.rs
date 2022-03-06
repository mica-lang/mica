//! Mica is an embeddable scripting language for Rust.
//!
//! This crate exposes a safe, high-level API akin to Rhai.

mod engine;
mod error;
mod fiber;
mod value;

pub use engine::*;
pub use error::*;
pub use fiber::*;
pub use value::*;

pub use mica_language as language;
pub use mica_language::value::{Type, Value};
