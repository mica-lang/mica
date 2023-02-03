use crate::{ll::value::Tuple, TypeBuilder};

pub(crate) fn define(size: usize, mut builder: TypeBuilder<Tuple>) -> TypeBuilder<Tuple> {
    for i in 0..size {
        let function_name = format!("_{i}");
        builder = builder.add_function(&function_name, move |t: &Tuple| t.fields[i]);
    }

    builder
}
