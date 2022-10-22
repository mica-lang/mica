use mica_hl::Engine;

use self::counters::load_counters;

mod counters;

pub(crate) fn load_iterators(engine: &mut Engine) -> Result<(), mica_hl::Error> {
    load_counters(engine)?;

    Ok(())
}
