use crate::{
    corelib::iterators::dict::DictIter, ll::value::Dict, Arguments, IntoValue,
    MethodParameterCount, RawFunctionKind, TypeBuilder,
};

pub(crate) fn define(builder: TypeBuilder<Dict>) -> TypeBuilder<Dict> {
    builder
        .add_function("len", Dict::len)
        .add_function("is_empty", Dict::is_empty)
        .add_function("insert", Dict::insert)
        .add_function("remove", Dict::remove)
        .add_function("get", Dict::get)
        .add_function("contains_key", Dict::contains_key)
        .add_function("clone", Dict::clone)
        // TODO: It should be possible to implement this without raw functions in the future.
        .add_raw_function(
            "iter",
            MethodParameterCount::from_count_with_self(1),
            RawFunctionKind::Foreign(Box::new(|library, gc, args| {
                let arguments = Arguments::new(args, library);
                let iter = unsafe { DictIter::new(*arguments.raw_self()) };
                Ok(iter.into_value_with_engine_state(library, gc).to_raw(gc))
            })),
        )
}
