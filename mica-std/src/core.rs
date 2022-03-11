//! Core functions.

use std::fmt;
use std::fmt::Write;

use mica_hl::{Arguments, Engine, MicaResultExt, Value};

fn print(arguments: Arguments) {
   for value in arguments.array() {
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

fn error(arguments: Arguments) -> Result<(), UserError> {
   let mut message = String::new();
   for value in arguments.array() {
      write!(message, "{}", value).unwrap();
   }
   Err(UserError(message))
}

fn assert(condition: Value, message: Option<Value>) -> Result<Value, mica_hl::Error> {
   if condition.is_falsy() {
      let message =
         message.map(|value| value.to_string()).unwrap_or_else(|| "assertion failed".to_owned());
      Err(message).mica()
   } else {
      Ok(condition)
   }
}

/// Loads the core library into the engine.
pub fn load_core(engine: &Engine) -> Result<(), mica_hl::Error> {
   engine.function("print", print)?;
   engine.function("error", error)?;
   engine.function("assert", assert)?;

   Ok(())
}
