use crate::{
    hl::value::UsesEngine,
    ll::{bytecode::Library, gc::Memory, value::Tuple},
    Gc, Hidden, IntoValue, Value,
};

macro_rules! value_from_tuple {
    ($($args:tt),*) => {
        impl<$($args,)*> IntoValue for ($($args,)*)
        where
            $($args: IntoValue),*
        {
            /// Constructing tuples requires having the engine, since we don't know whether any
            /// of the inner values will need the engine or not. Unfortunately Rust's type system
            /// is not expressive enough to let us express "if at least one value requires the
            /// engine, then [`IntoValue`] should require the engine."
            type EngineUse = UsesEngine;

            fn into_value(self, (library, gc): (&Library, &mut Memory)) -> Value {
                #[allow(non_snake_case)]
                let ($($args),*,) = self;
                Value::Tuple(Hidden(Gc::new(Box::new(Tuple::new(vec![
                    $($args.into_value_with_engine_state(library, gc).to_raw(gc)),*
                ])))))
            }
        }
    }
}

value_from_tuple!(A);
value_from_tuple!(A, B);
value_from_tuple!(A, B, C);
value_from_tuple!(A, B, C, D);
value_from_tuple!(A, B, C, D, E);
value_from_tuple!(A, B, C, D, E, F);
value_from_tuple!(A, B, C, D, E, F, G);
value_from_tuple!(A, B, C, D, E, F, G, H);
