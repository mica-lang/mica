use self::{
    bytes::load_string_bytes_iter, chars::load_string_chars_iter,
    code_points::load_string_code_points_iter, lines::load_string_lines_iter,
    rsplit::load_string_rsplit_iter, split::load_string_split_iter,
};
use crate::{Engine, Error};

pub mod bytes;
pub mod chars;
pub mod code_points;
pub mod lines;
pub mod rsplit;
pub mod split;

pub(crate) fn load_string_iterators(engine: &mut Engine) -> Result<(), Error> {
    load_string_bytes_iter(engine)?;
    load_string_chars_iter(engine)?;
    load_string_code_points_iter(engine)?;
    load_string_lines_iter(engine)?;
    load_string_split_iter(engine)?;
    load_string_rsplit_iter(engine)?;

    Ok(())
}
