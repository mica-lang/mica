#![allow(unused)]

use mica_hl::{Engine, Error, Value};

fn engine() -> Result<Engine, Error> {
   let mut engine = Engine::new(mica_std::lib());
   mica_std::load(&mut engine)?;
   Ok(engine)
}

fn interpret<'e>(
   engine: &'e mut Engine,
   filename: &str,
   input: &str,
) -> Result<impl Iterator<Item = Result<Value, Error>> + 'e, Error> {
   let mut fiber = match engine.start(filename, input) {
      Ok(fiber) => fiber,
      Err(error) => {
         eprintln!("{error}");
         return Ok(None.into_iter().flatten());
      }
   };
   Ok(Some(std::iter::from_fn(move || match fiber.resume() {
      Ok(Some(value)) => Some(Ok(value)),
      Ok(None) => None,
      Err(error) => {
         eprintln!("{error}");
         Some(Err(error))
      }
   }))
   .into_iter()
   .flatten())
}

#[test]
fn gc_collect_shouldnt_cause_use_after_free() -> Result<(), Error> {
   let mut engine = engine()?;

   for result in interpret(&mut engine, "(repl)", "[]")? {
      println!("< {result:?}");
      println!();
   }
   for result in interpret(&mut engine, "(repl)", "[]")? {
      println!("< {result:?}");
      println!();
   }
   for result in interpret(&mut engine, "(repl)", "Gc.collect")? {
      println!("< {result:?}");
      println!();
   }

   Ok(())
}
