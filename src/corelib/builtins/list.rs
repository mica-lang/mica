use crate::{
    corelib::iterators::list::ListIter,
    ll::value::{List, RawValue},
    Arguments, IntoValue, MethodParameterCount, RawFunctionKind, TypeBuilder,
};

pub(crate) fn define(builder: TypeBuilder<Vec<RawValue>>) -> TypeBuilder<Vec<RawValue>> {
    builder
        .add_function("len", Vec::len)
        .add_function("is_empty", Vec::is_empty)
        // get is a common operation and is thus implemented as a raw function to avoid a roundtrip
        // conversion to and from a safe Value, which is not zero-cost.
        .add_raw_function(
            "get",
            MethodParameterCount::from_count_with_self(2),
            RawFunctionKind::Foreign(Box::new(|env, _, args| {
                let arguments = Arguments::new(args, env);
                let v = unsafe {
                    arguments
                        .raw_self()
                        .downcast_user_data_unchecked::<List>()
                        .as_slice()
                };
                let index = arguments.nth(0).unwrap().ensure_number()? as usize;
                Ok(v.get(index).copied().unwrap_or(RawValue::from(())))
            })),
        )
        .add_function(
            "set",
            |v: &mut Vec<RawValue>, index: usize, value: RawValue| {
                if index < v.len() {
                    v[index] = value;
                    Ok(value)
                } else {
                    Err(OutOfBounds {
                        index,
                        len: v.len(),
                    })
                }
            },
        )
        .add_function("first", |v: &Vec<RawValue>| v.first().copied())
        .add_function("last", |v: &Vec<RawValue>| v.last().copied())
        .add_function("contains", |v: &Vec<RawValue>, x: RawValue| v.contains(&x))
        .add_function("clear", Vec::clear)
        .add_function("dedup", Vec::dedup)
        .add_function("insert", Vec::insert)
        .add_function("remove", Vec::remove)
        .add_function("swap_remove", Vec::swap_remove)
        .add_function("push", Vec::push)
        .add_function("pop", Vec::pop)
        .add_function("resize", Vec::resize)
        .add_function("truncate", Vec::truncate)
        .add_function("repeat", |v: &Vec<RawValue>, n: usize| v.repeat(n))
        .add_function("reverse", |v: &mut Vec<RawValue>| v.reverse())
        .add_function("rotate_left", |v: &mut Vec<RawValue>, n: usize| {
            v.rotate_left(n)
        })
        .add_function("rotate_right", |v: &mut Vec<RawValue>, n: usize| {
            v.rotate_right(n)
        })
        .add_function("swap", |v: &mut Vec<RawValue>, a: usize, b: usize| {
            v.swap(a, b)
        })
        .add_function("clone", |v: &Vec<RawValue>| v.clone())
        // TODO: It should be possible to implement this without raw functions in the future.
        .add_raw_function(
            "iter",
            MethodParameterCount::from_count_with_self(1),
            RawFunctionKind::Foreign(Box::new(|library, gc, args| {
                let arguments = Arguments::new(args, library);
                let iter = unsafe { ListIter::new(*arguments.raw_self()) };
                Ok(iter.into_value_with_engine_state(library, gc).to_raw(gc))
            })),
        )
}

#[derive(Debug)]
struct OutOfBounds {
    index: usize,
    len: usize,
}

impl std::fmt::Display for OutOfBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "index {} is out of the list's bounds (the list's length is {})",
            self.index, self.len
        )
    }
}

impl std::error::Error for OutOfBounds {}
