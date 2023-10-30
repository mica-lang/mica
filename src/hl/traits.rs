use std::{fmt, rc::Rc};

use crate::{
    ll::{
        bytecode::{Environment, TraitIndex},
        codegen,
        gc::{Gc, Memory},
        value::create_trait,
    },
    Error, Hidden, LanguageErrorKind, MethodId, MethodParameterCount, Value,
};

/// Allows you to build traits programatically from Rust code.
///
/// Using traits is the intended way of calling Mica methods from Rust, because it's faster
/// (involving less indirections) and avoids calling unrelated methods unintentionally, because
/// every type defined in Mica must explicitly opt into implementing a trait.
///
/// # Limitations
///
/// Right now there exists no interface for implementing trait functions from within Rust for any
/// traits other than [the built-ins][crate::builtin_traits], and it's possible that there will
/// never be a way of doing so without violating type safety, which would increase the risk of bugs.
///
/// That said, this not a hard "no;" it just hasn't been researched nor implemented at the moment.
///
/// # Example
/// ```
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use mica::{Engine, TraitBuilder, Value};
///
/// let mut engine = Engine::new();
///
/// let mut game_builder = engine.build_trait("Game")?;
/// let game_tick = game_builder.add_function("tick", 0)?;
/// let game_tick_count = game_builder.add_function("tick_count", 0)?;
/// let game = game_builder.build();
/// engine.set("Game", game);
///
/// let my_game: Value = engine
///     .start(
///         "game.mi",
///         r#" let tick_count = 0
///
///             struct MyGame impl
///                 func new() constructor = nil
///
///                 as Game
///                     func tick() =
///                         tick_count = tick_count + 1
///
///                     func tick_count() = tick_count
///                 end
///             end
///             .new() "#
///     )?
///     .trampoline()?;
///
/// let expected_tick_count = 10_usize;
/// for _ in 0..expected_tick_count {
///     let _: Value = engine.call_method(my_game.clone(), game_tick, [])?;
/// }
/// let got_tick_count: usize = engine.call_method(my_game, game_tick_count, [])?;
/// assert_eq!(got_tick_count, expected_tick_count);
/// # Ok(())
/// # }
/// ```
pub struct TraitBuilder<'e> {
    pub(crate) inner: codegen::TraitBuilder<'e>,
    pub(crate) gc: &'e mut Memory,
}

impl<'e> TraitBuilder<'e> {
    /// Adds a new function requirement into the trait and returns its method ID, which can be used
    /// to call the function on values implementing the trait.
    pub fn add_function(&mut self, name: &str, arity: u8) -> Result<MethodId, Error> {
        let arity = MethodParameterCount::from_count_without_self(arity)
            .map_err(|_| Error::TooManyParametersInTraitMethod)?;
        self.inner
            .add_method(Rc::from(name), arity)
            .map(MethodId)
            .map_err(|e| match e {
                LanguageErrorKind::TooManyTraits => Error::TooManyTraits,
                LanguageErrorKind::TooManyFunctions => Error::TooManyFunctions,
                LanguageErrorKind::TooManyMethods => Error::TooManyMethods,
                LanguageErrorKind::TooManyParameters => Error::TooManyParametersInTraitMethod,
                _ => unreachable!(),
            })
    }

    /// Finishes building the trait and wraps it into a value.
    pub fn build(self) -> Value {
        let (trait_id, env) = self.inner.build();
        create_trait_value(env, self.gc, trait_id)
    }
}

impl<'e> fmt::Debug for TraitBuilder<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TraitBuilder").finish_non_exhaustive()
    }
}

pub(crate) fn create_trait_value(
    env: &mut Environment,
    gc: &mut Memory,
    trait_id: TraitIndex,
) -> Value {
    let instance = create_trait(env, gc, trait_id);
    let instance = unsafe { Gc::from_raw(instance) };
    Value::Trait(Hidden(instance))
}
