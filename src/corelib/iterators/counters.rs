//! `CountUp` and `CountDown` iterators.

use crate::{builtin_traits::iterator, Engine, Error, TypeBuilder, UserData};

struct CountUp {
    current: i64,
    max: i64,
    step: i64,
}

impl CountUp {
    fn with_step(min: i64, max: i64, step: i64) -> Self {
        Self { current: min, max, step }
    }

    fn new(min: i64, max: i64) -> Self {
        Self::with_step(min, max, 1)
    }

    fn has_next(&self) -> bool {
        self.current <= self.max
    }

    fn next(&mut self) -> i64 {
        let i = self.current;
        self.current += self.step;
        i
    }
}

impl UserData for CountUp {}

struct CountDown {
    current: i64,
    min: i64,
    step: i64,
}

impl CountDown {
    fn with_step(max: i64, min: i64, step: i64) -> Self {
        Self { current: max, min, step }
    }

    fn new(max: i64, min: i64) -> Self {
        Self::with_step(max, min, 1)
    }

    fn has_next(&self) -> bool {
        self.current >= self.min
    }

    fn next(&mut self) -> i64 {
        let i = self.current;
        self.current -= self.step;
        i
    }
}

impl UserData for CountDown {}

pub(crate) fn load_counters(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<CountUp>::new("CountUp")
            .add_constructor("new", |ctor| {
                move |min, max, step| ctor.construct(CountUp::with_step(min, max, step))
            })
            .add_constructor("new", |ctor| move |min, max| ctor.construct(CountUp::new(min, max)))
            .add_builtin_trait_function(iterator::HasNext, CountUp::has_next)
            .add_builtin_trait_function(iterator::Next, CountUp::next),
    )?;

    engine.add_type(
        TypeBuilder::<CountDown>::new("CountDown")
            .add_constructor("new", |ctor| {
                move |max, min, step| ctor.construct(CountDown::with_step(max, min, step))
            })
            .add_constructor("new", |ctor| move |max, min| ctor.construct(CountDown::new(max, min)))
            .add_builtin_trait_function(iterator::HasNext, CountDown::has_next)
            .add_builtin_trait_function(iterator::Next, CountDown::next),
    )?;

    engine.start("mstd-counters.mi", include_str!("counters.mi"))?.trampoline()?;

    Ok(())
}
