//! Mica is an embeddable scripting language for Rust.
//!
//! This crate exposes a safe, high-level API akin to Rhai.

mod engine;
mod error;
mod fiber;

pub use engine::*;
pub use error::*;
pub use fiber::*;

pub use mica_language as language;
pub use mica_language::value::Value;
