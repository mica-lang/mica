//! The bytecode representation of Mica.

mod chunk;
mod dispatch_table;
mod environment;
mod function;
mod impls;
mod library;
mod opcode;
mod opr24;

pub use self::{
    chunk::*, dispatch_table::*, environment::*, function::*, impls::*, library::*, opcode::*,
    opr24::*,
};
