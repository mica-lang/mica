use crate::{ll::value::RawValue, Engine, Error, TypeBuilder, UserData};

/// A pair of two raw values.
#[derive(Debug, Clone, Copy)]
pub struct Pair {
    /// The first value.
    pub a: RawValue,
    /// The second value.
    pub b: RawValue,
}

pub(crate) fn load_pair(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<Pair>::new("Pair")
            .add_static("new", |a: RawValue, b: RawValue| Pair { a, b })
            .add_function("first", |pair: &Pair| pair.a)
            .add_function("key", |pair: &Pair| pair.a)
            .add_function("left", |pair: &Pair| pair.a)
            .add_function("second", |pair: &Pair| pair.b)
            .add_function("value", |pair: &Pair| pair.b)
            .add_function("right", |pair: &Pair| pair.b),
    )?;
    engine.add_function("pair", |a: RawValue, b: RawValue| Pair { a, b })?;

    Ok(())
}

impl UserData for Pair {
    fn visit_references(&self, visit: &mut dyn FnMut(RawValue)) {
        visit(self.a);
        visit(self.b);
    }
}
