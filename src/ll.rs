//! The **l**ow-**l**evel (`ll`) implementation of the Mica programming language.
//!
//! You usually want to use the [high-level API][crate] instead. This API is **much more unstable**
//! and may not be documented as well as the high-level API.

// See? I told you it's not as documented.
// And it doesn't have to be.
#![allow(missing_docs)]

pub mod ast;
pub mod bytecode;
pub mod codegen;
pub mod error;
pub mod gc;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod vm;
