// We allow unreachable `pub` items in this module because it's easier to add modules via
// rust-analyzer that way (which does not have an assist for creating a `pub(crate)` module at the
// moment.)
#![allow(unreachable_pub)]

use self::{
    counters::load_counters, dict::load_dict_iter, list::load_list_iter,
    string::load_string_iterators,
};
use crate::{Engine, Error};

mod counters;
pub mod dict;
pub mod list;
pub mod string;

pub(crate) fn load_iterators(engine: &mut Engine) -> Result<(), Error> {
    load_counters(engine)?;
    load_list_iter(engine)?;
    load_dict_iter(engine)?;
    load_string_iterators(engine)?;

    Ok(())
}
