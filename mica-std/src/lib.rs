//! The Mica standard library. Provides functions that can be registered in engines.

mod builtins;
mod core;
mod io;

use mica_hl::Engine;

pub use crate::builtins::lib;
pub use crate::core::load_core;
pub use crate::io::load_io;

/// Loads the full standard library into the engine.
pub fn load(engine: &mut Engine) -> Result<(), mica_hl::Error> {
   load_core(engine)?;
   load_io(engine)?;

   Ok(())
}
