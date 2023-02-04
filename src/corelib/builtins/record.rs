use crate::{ll::value::Record, TypeBuilder};

pub(crate) fn define<'a>(
    fields: impl Iterator<Item = (&'a str, usize)>,
    mut builder: TypeBuilder<Record>,
) -> TypeBuilder<Record> {
    for (name, index) in fields {
        builder = builder.add_function(name, move |record: &Record| record.fields[index]);
    }

    builder
}
