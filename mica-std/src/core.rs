//! Core functions.

use std::fmt;
use std::fmt::Write;

use mica_hl::{Arguments, Engine, Value};

fn print(arguments: Arguments) {
   for value in arguments.raw() {
      print!("{}", value);
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

fn error(arguments: Arguments) -> Result<Value, UserError> {
   let mut message = String::new();
   for value in arguments.raw() {
      write!(message, "{}", value).unwrap();
   }
   Err(UserError(message))
}

/// Loads the core library into the engine.
pub fn load_core(engine: &Engine) -> Result<(), mica_hl::Error> {
   engine.function("print", print)?;
   engine.function("error", error)?;

   Ok(())
}
