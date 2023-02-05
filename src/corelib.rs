//! The Mica core library. Provides the fundamental set of functions and types.

use self::{builtins::*, core::load_core};
use crate::{
    ll::value::{Dict, RawValue, Record, Tuple},
    CoreLibrary, Engine, Error, TypeBuilder,
};

mod builtins;
mod core;
mod gc;
mod iterators;

/// Unit struct representing the core library.
#[derive(Debug, Clone, Copy)]
pub struct Lib;

impl CoreLibrary for Lib {
    fn define_nil(&self, builder: TypeBuilder<()>) -> TypeBuilder<()> {
        builder
    }

    fn define_boolean(&self, builder: TypeBuilder<bool>) -> TypeBuilder<bool> {
        builder
    }

    fn define_number(&self, builder: TypeBuilder<f64>) -> TypeBuilder<f64> {
        number::define(builder)
    }

    fn define_string(&self, builder: TypeBuilder<String>) -> TypeBuilder<String> {
        string::define(builder)
    }

    fn define_list(&self, builder: TypeBuilder<Vec<RawValue>>) -> TypeBuilder<Vec<RawValue>> {
        list::define(builder)
    }

    fn define_dict(&self, builder: TypeBuilder<Dict>) -> TypeBuilder<Dict> {
        dict::define(builder)
    }

    fn define_tuple(&self, size: usize, builder: TypeBuilder<Tuple>) -> TypeBuilder<Tuple> {
        tuple::define(size, builder)
    }

    fn define_record<'a>(
        &self,
        fields: impl Iterator<Item = (&'a str, usize)>,
        builder: TypeBuilder<Record>,
    ) -> TypeBuilder<Record> {
        record::define(fields, builder)
    }

    fn load(&self, engine: &mut Engine) -> Result<(), Error> {
        load_core(engine)
    }
}
