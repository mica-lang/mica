//! The Mica standard library. Provides functions that can be registered in engines.

#![allow(clippy::or_fun_call)]

mod builtins;
mod core;
mod gc;
#[cfg(feature = "io")]
mod io;
mod iterators;

use mica_hl::Engine;

#[cfg(feature = "io")]
pub use crate::io::load_io;
pub use crate::{builtins::lib, core::load_core};

/// Loads the full standard library into the engine.
pub fn load(engine: &mut Engine) -> Result<(), mica_hl::Error> {
    load_core(engine)?;
    #[cfg(feature = "io")]
    load_io(engine)?;

    Ok(())
}
