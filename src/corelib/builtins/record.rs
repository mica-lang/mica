use crate::{ll::value::Record, TypeBuilder};

pub(crate) fn define<'a>(
    fields: impl Iterator<Item = (&'a str, usize)>,
    mut builder: TypeBuilder<Record>,
) -> TypeBuilder<Record> {
    builder
}
