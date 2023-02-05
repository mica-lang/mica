//! This module contains the safe, high-level API of Mica.

pub mod builtin_traits;
mod corelib;
mod engine;
mod error;
mod fiber;
mod function;
mod traits;
mod types;
mod userdata;
mod value;

mod generated;

pub use corelib::*;
pub use engine::*;
pub use error::*;
pub use fiber::*;
pub use function::*;
pub use traits::*;
pub use types::*;
pub use userdata::*;
pub use value::*;

pub use crate::ll::gc::Gc;
