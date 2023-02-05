use crate::{
    hl::value::type_mismatch,
    ll::{bytecode::Library, value::Tuple},
    Error, Hidden, TryFromValue, Value,
};

macro_rules! try_from_value_tuple {
    (count = $count:tt, $($args:tt),*) => {
        impl<$($args,)*> TryFromValue for ($($args,)*)
        where
            $($args: TryFromValue,)*
        {
            #[allow(non_snake_case)]
            fn try_from_value(value: &Value, library: &Library) -> Result<Self, Error> {
                if let Value::Tuple(Hidden(u)) = value {
                    let tuple = unsafe { u.as_any().downcast_ref::<Tuple>().unwrap_unchecked() };
                    return if tuple.fields.len() != $count {
                        Err(type_mismatch(concat!("Tuple(", stringify!($count), ")"), value))
                    } else {
                        let _n = 0;
                        $(
                            let $args = $args::try_from_value(
                                &Value::from_raw(unsafe { *tuple.fields.get_unchecked(_n) }),
                                library,
                            )?;
                            let _n = _n + 1;
                        )*
                        Ok(($($args,)*))
                    };
                }
                Err(type_mismatch(concat!("Tuple(", stringify!(1), ")"), value))
            }
        }
    };
}

try_from_value_tuple!(count = 1, A);
try_from_value_tuple!(count = 2, A, B);
try_from_value_tuple!(count = 3, A, B, C);
try_from_value_tuple!(count = 4, A, B, C, D);
try_from_value_tuple!(count = 5, A, B, C, D, E);
try_from_value_tuple!(count = 6, A, B, C, D, E, F);
try_from_value_tuple!(count = 7, A, B, C, D, E, F, G);
try_from_value_tuple!(count = 8, A, B, C, D, E, F, G, H);
