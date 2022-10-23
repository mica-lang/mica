//! The Mica core library. Provides the fundamental set of functions and types.

use self::{builtins::*, core::load_core};
use crate::{ll::value::Dict, CoreLibrary, Engine, Error, RawValue, TypeBuilder};

pub mod builtins;
pub mod core;
pub mod gc;
pub mod iterators;

/// Unit struct representing the core library.
pub struct Lib;

impl CoreLibrary for Lib {
    fn define_nil(&mut self, builder: TypeBuilder<()>) -> TypeBuilder<()> {
        builder
    }

    fn define_boolean(&mut self, builder: TypeBuilder<bool>) -> TypeBuilder<bool> {
        builder
    }

    fn define_number(&mut self, builder: TypeBuilder<f64>) -> TypeBuilder<f64> {
        number::define(builder)
    }

    fn define_string(&mut self, builder: TypeBuilder<String>) -> TypeBuilder<String> {
        string::define(builder)
    }

    fn define_list(&mut self, builder: TypeBuilder<Vec<RawValue>>) -> TypeBuilder<Vec<RawValue>> {
        list::define(builder)
    }

    fn define_dict(&mut self, builder: TypeBuilder<Dict>) -> TypeBuilder<Dict> {
        dict::define(builder)
    }

    fn load(&mut self, engine: &mut Engine) -> Result<(), Error> {
        load_core(engine)
    }
}
