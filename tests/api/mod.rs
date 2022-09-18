use std::fmt::Display;

use mica::Engine;

mod traits;

pub trait RevealResultExt<T> {
   /// Basically the same as `unwrap()` but `Display`s the error instead of `Debug`ging it.
   fn reveal(self) -> T;
}

impl<T, E> RevealResultExt<T> for Result<T, E>
where
   E: Display,
{
   fn reveal(self) -> T {
      match self {
         Ok(ok) => ok,
         Err(error) => {
            panic!("Err result revealed:\n\n{error}\n\n");
         }
      }
   }
}

pub fn create_engine() -> Engine {
   let mut engine = Engine::new(mica::std::lib());
   mica::std::load(&mut engine).expect("cannot load stdlib");
   engine
}
