use self::counters::load_counters;
use crate::{Engine, Error};

mod counters;

pub(crate) fn load_iterators(engine: &mut Engine) -> Result<(), Error> {
    load_counters(engine)?;

    Ok(())
}
