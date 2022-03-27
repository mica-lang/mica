//! Mica is an embeddable scripting language for Rust.
//!
//! This crate exposes a safe, high-level API akin to Rhai.

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
pub use mica_language::value::{Type, Value};
