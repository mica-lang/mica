//! Core functions.

use std::fmt;
use std::fmt::Write;

use mica_hl::{Engine, LanguageErrorKind, Value};

fn print(arguments: &[Value]) -> Result<Value, LanguageErrorKind> {
   for value in arguments {
      print!("{}", value);
   }
   println!();
   Ok(Value::Nil)
}

#[derive(Debug)]
struct UserError(String);

impl fmt::Display for UserError {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self.0)
   }
}

impl std::error::Error for UserError {}

fn error(arguments: &[Value]) -> Result<Value, LanguageErrorKind> {
   let mut message = String::new();
   for value in arguments {
      write!(message, "{}", value).unwrap();
   }
   Err(LanguageErrorKind::User(Box::new(UserError(message))))
}

/// Loads the core library into the engine.
pub fn load_core(engine: &Engine) -> Result<(), mica_hl::Error> {
   engine.raw_function("print", None, Box::new(print))?;
   engine.raw_function("error", None, Box::new(error))?;

   Ok(())
}
