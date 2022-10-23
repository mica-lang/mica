//! Types for representing implementations of built-in traits.
//!
//! Note that when implementing a built-in trait, _you must ensure_ that you implement _all_ the
//! methods. The high-level API does not guard against leaving some methods unimplemented, like the
//! language interpreter does.

use crate::ffvariants::{FallibleSelf, ImmutableSelf, InfallibleSelf, MutableSelf};

/// Enumeration of all available built-in traits.
#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub enum BuiltinTrait {
    None,
    Iterator,
}

mod private {
    pub trait Sealed {}
}

/// A built-in trait method that can be implemented with functions whose signature fits `S`.
pub trait BuiltinTraitFunction<S>: private::Sealed {
    #[doc(hidden)]
    const NAME: &'static str;

    #[doc(hidden)]
    fn owning_trait(&self) -> BuiltinTrait;
}

macro_rules! implement_builtin_trait_function {
    ($T:ty, $name:expr, ($($Args:ty),*), $owning_trait:expr) => {
        impl super::private::Sealed for $T {}

        macro_rules! implement_for {
            (<$S:tt> $ST:ty) => {
                impl<$S> BuiltinTraitFunction<$ST> for $T {
                    const NAME: &'static str = $name;

                    fn owning_trait(&self) -> BuiltinTrait {
                        $owning_trait
                    }
                }
            };
        }

        implement_for!(<S> FallibleSelf<ImmutableSelf<S>, (&S, $($Args),*)>);
        implement_for!(<S> InfallibleSelf<ImmutableSelf<S>, (&S, $($Args),*)>);
        implement_for!(<S> InfallibleSelf<MutableSelf<S>, (&mut S, $($Args),*)>);
        implement_for!(<S> FallibleSelf<MutableSelf<S>, (&mut S, $($Args),*)>);
    };
}

/// Methods from the `Iterator` trait.
pub mod iterator {
    use super::*;

    /// `Iterator.has_next`, can be implemented with `fn (&mut self) -> T`.
    #[derive(Debug)]
    pub struct HasNext;
    implement_builtin_trait_function!(HasNext, "has_next", (), BuiltinTrait::Iterator);

    /// `Iterator.next`, can be implemented with `fn (&mut self) -> T`.
    #[derive(Debug)]
    pub struct Next;
    implement_builtin_trait_function!(Next, "next", (), BuiltinTrait::Iterator);
}
