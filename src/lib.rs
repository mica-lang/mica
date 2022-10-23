#![allow(clippy::or_fun_call)]
#![warn(missing_docs, missing_debug_implementations, unreachable_pub)]
#![doc = include_str!("lib.md")]

pub mod corelib;
mod hl;
pub mod ll;

pub use hl::*;
