//! Core functions.

use mica_hl::{Engine, Value};

fn print(arguments: &[Value]) -> Value {
   for value in arguments {
      print!("{}", value);
   }
   println!();
   Value::Nil
}

/// Loads the core library into the engine.
pub fn load_core(engine: &Engine) -> Result<(), mica_hl::Error> {
   engine.raw_function("print", None, Box::new(print))?;

   Ok(())
}
