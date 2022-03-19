//! The Mica standard library. Provides functions that can be registered in engines.

mod builtins;
mod core;

use mica_hl::Engine;

pub use crate::builtins::lib;
pub use crate::core::load_core;

/// Loads the full standard library into the engine.
pub fn load(engine: &Engine) -> Result<(), mica_hl::Error> {
   load_core(engine)?;

   Ok(())
}
