//! The low-level implementation of the Mica programming language.
//! You usually want to use the [high-level API][crate] instead.

pub mod ast;
pub mod bytecode;
pub mod codegen;
pub mod common;
pub mod gc;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod vm;