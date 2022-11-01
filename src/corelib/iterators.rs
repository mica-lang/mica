use self::{counters::load_counters, list::load_list_iter};
use crate::{Engine, Error};

mod counters;
pub(crate) mod list;

pub(crate) fn load_iterators(engine: &mut Engine) -> Result<(), Error> {
    load_counters(engine)?;
    load_list_iter(engine)?;

    Ok(())
}
