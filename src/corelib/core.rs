//! Core functions.

use std::{fmt, fmt::Write};

use super::pair::load_pair;
use crate::{
    corelib::{gc::load_gc, iterators::load_iterators},
    Arguments, Engine, Error, MicaResultExt, Value,
};

fn print(arguments: Arguments) {
    for value in arguments.array() {
        print!("{value}");
    }
    println!();
}

fn debug(arguments: Arguments) {
    for (i, value) in arguments.array().iter().enumerate() {
        if i > 0 {
            print!("\t");
        }
        print!("{value:?}");
    }
    println!();
}

#[derive(Debug)]
struct UserError(String);

impl fmt::Display for UserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for UserError {}

fn error(arguments: Arguments) -> Result<(), UserError> {
    let mut message = String::new();
    for value in arguments.array() {
        write!(message, "{value}").unwrap();
    }
    Err(UserError(message))
}

fn assert(condition: Value, message: Option<Value>) -> Result<Value, Error> {
    if condition.is_falsy() {
        let message =
            message.map(|value| value.to_string()).unwrap_or_else(|| "assertion failed".to_owned());
        Err(message).mica()
    } else {
        Ok(condition)
    }
}

/// Loads the core library into the engine.
pub(crate) fn load_core(engine: &mut Engine) -> Result<(), Error> {
    engine.add_function("print", print)?;
    engine.add_function("debug", debug)?;
    engine.add_function("error", error)?;
    engine.add_function("assert", assert)?;

    load_gc(engine)?;
    load_pair(engine)?;
    load_iterators(engine)?;

    Ok(())
}
