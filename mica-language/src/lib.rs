//! The implementation of the Mica programming language. This crate contains the low-level API;
//! you usually want to use the high-level API from [`mica-hl`](https://crates.io/crates/mica-hl)
//! instead.

#![allow(clippy::or_fun_call)]

pub mod ast;
pub mod bytecode;
pub mod codegen;
pub mod common;
pub mod gc;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod vm;
