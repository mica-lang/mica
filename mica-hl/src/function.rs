use mica_language::{gc::Memory, value::RawValue};

use crate::{Error, LanguageErrorKind, RawForeignFunction, TryFromValue, Value};

fn create_rawff(
    f: impl FnMut(&mut Memory, &[RawValue]) -> Result<RawValue, LanguageErrorKind> + 'static,
) -> RawForeignFunction {
    Box::new(f)
}

/// Arguments passed to a varargs function.
///
/// This is a wrapper that does things like type-checking and arity-checking.
pub struct Arguments<'a> {
    this: RawValue,
    inner: &'a [RawValue],
}

impl<'a> Arguments<'a> {
    pub fn new(raw_arguments: &'a [RawValue]) -> Self {
        // Skip the first argument, which is `self` (or the currently called function).
        Self { this: raw_arguments[0], inner: &raw_arguments[1..] }
    }

    /// Returns the number of arguments passed to the function.
    pub fn count(&self) -> usize {
        self.inner.len()
    }

    /// Raises an error if there weren't exactly `n` arguments passed to the function.
    pub fn expect_exactly(&self, n: usize) -> Result<(), Error> {
        if self.count() != n {
            Err(Error::ArgumentCount { expected: n, got: self.count() })
        } else {
            Ok(())
        }
    }

    /// Raises an error if there weren't at least `n` arguments passed to the function.
    pub fn expect_at_least(&self, n: usize) -> Result<(), Error> {
        if self.count() < n {
            Err(Error::ArgumentCount { expected: n, got: self.count() })
        } else {
            Ok(())
        }
    }

    /// Returns the `self` parameter as a value.
    pub fn raw_self(&self) -> &RawValue {
        &self.this
    }

    /// Returns the `n`th argument, or `None` if the argument is not present.
    pub fn nth(&self, n: usize) -> Option<&RawValue> {
        self.inner.get(n)
    }

    /// Returns the `n`th argument converted into the given type. Returns an error if there aren't
    /// enough arguments.
    pub fn get<T>(&self, n: usize) -> Result<T, Error>
    where
        T: TryFromValue,
    {
        let value = self.inner.get(n).cloned().unwrap_or(RawValue::from(()));
        T::try_from_value(&Value::from_raw(value)).map_err(|error| {
            if let Error::TypeMismatch { expected, got } = error {
                Error::ArgumentTypeMismatch { index: n, expected, got }
            } else {
                error
            }
        })
    }

    /// Returns the raw array of arguments.
    pub fn array(&self) -> &[RawValue] {
        self.inner
    }
}

/// Wrapper struct for marking functions that use the with-raw-self calling convention.
///
/// This `Deref`s to the inner value.
#[repr(transparent)]
pub struct RawSelf<'a>(&'a RawValue);

impl std::ops::Deref for RawSelf<'_> {
    type Target = RawValue;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> From<&'a RawValue> for RawSelf<'a> {
    fn from(value: &'a RawValue) -> Self {
        Self(value)
    }
}

/// A Rust function that can be called from Mica.
///
/// To spare you the time needed to decipher the absolutely unreadable macro-generated
/// implementations of this trait, functions with the following signatures are supported:
/// - Constant number of type checked arguments
///   - `fn (A, B, C, ...) -> R` where
///     - Each argument: [`TryFromValue`]
///     - `R`: [`Into`]`<`[`Value`]`>`
///   - `fn (A, B, C, ...) -> Result<R, E>`
///     - Each argument: [`TryFromValue`]
///     - `R`: [`Into`]`<`[`Value`]`>`
///   - `fn (Self, A, B, C, ...) -> R` where
///     - `Self`: [`SelfFromRawValue`][`crate::SelfFromRawValue`] or
///       [`MutSelfFromRawValue`][`crate::MutSelfFromRawValue`]
///     - Each argument after `Self`: [`TryFromValue`]
///     - `R`: [`Into`]`<`[`Value`]`>`
///   - `fn (Self, A, B, C, ...) -> Result<R, E>`
///     - `Self`: [`SelfFromRawValue`][`crate::SelfFromRawValue`] or
///       [`MutSelfFromRawValue`][`crate::MutSelfFromRawValue`]
///     - Each argument after `Self`: [`TryFromValue`]
///     - `R`: [`Into`]`<`[`Value`]`>`
///   - Due to a limitation in Rust's type system, a maximum of 8 arguments is supported now. If
///     more is needed, use the varargs versions described below.
/// - Variable number of dynamically typed arguments
///   - `fn (`[`Arguments`]`) -> R` where `R`: [`Into`]`<`[`Value`]`>`
///   - `fn (`[`Arguments`]`) -> Result<R, E>` where
///     - `R`: [`Into`]`<`[`Value`]`>`
///     - `E`: [`std::error::Error`]
///
/// The generic parameter `V` is not used inside the trait. Its only purpose is to allow for
/// multiple overlapping implementations of a trait for the same type. See [`ffvariants`] for more
/// information.
pub trait ForeignFunction<V> {
    /// Returns the number of parameters this function has, or `None` if the function accepts a
    /// variable number of arguments.
    ///
    /// The default implementation returns `None`.
    fn parameter_count() -> Option<u16>;

    /// Converts the function to a `RawForeignFunction`.
    fn into_raw_foreign_function(self) -> RawForeignFunction;
}

/// Variants of `ForeignFunction`.
///
/// This is a bit of a hack around Rust's type system not supporting disjoint generic
/// implementations. Each implementation has a corresponding marker zero variant enum in this
/// module, such that eg. the traits `ForeignFunction<foreign::GenericResult>` and
/// `ForeignFunction<foreign::Infallible>` are different, but can both be matched by using a generic
/// parameter.
pub mod ffvariants {
    use std::marker::PhantomData;

    // For the type system to accept our implemenations, the types of the function's parameters must
    // appear somewhere in the trait or the implemented self type. We can't control the self type,
    // so we put them into a generic parameter here.
    // The PhantomData inside suppresses an "unused generic parameter" error and prevents
    // construction of the two structs (because it's a private field).

    /// A bare fallible function.
    pub struct Fallible<Args>(PhantomData<Args>);
    /// A bare infallible function.
    pub struct Infallible<Args>(PhantomData<Args>);
    /// A bare varargs fallible function.
    pub enum VarargsFallible {}
    /// A bare varargs infallible function.
    pub enum VarargsInfallible {}

    mod bare {
        pub trait Sealed {}

        impl<Args> Sealed for super::Fallible<Args> {}
        impl<Args> Sealed for super::Infallible<Args> {}
        impl Sealed for super::VarargsFallible {}
        impl Sealed for super::VarargsInfallible {}
    }

    /// Marker trait for all functions that _don't_ accept a `self` reference as the first
    /// parameter.
    ///
    /// See also [`Method`].
    pub trait Bare: bare::Sealed {}

    impl<Args> Bare for Fallible<Args> {}
    impl<Args> Bare for Infallible<Args> {}
    impl Bare for VarargsFallible {}
    impl Bare for VarargsInfallible {}

    // S is the self type (`ImmutableSelf` or `MutableSelf`).
    /// A fallible function with `RawSelf`.
    pub struct FallibleRawSelf<Args>(PhantomData<Args>);
    /// An infallible function with `RawSelf`.
    pub struct InfallibleRawSelf<Args>(PhantomData<Args>);
    /// A fallible function with typed `self`.
    pub struct FallibleSelf<S, Args>(PhantomData<(S, Args)>);
    /// An infallible function with typed `self`.
    pub struct InfallibleSelf<S, Args>(PhantomData<(S, Args)>);

    mod method {
        pub trait Sealed {}

        impl<Args> Sealed for super::FallibleRawSelf<Args> {}
        impl<Args> Sealed for super::InfallibleRawSelf<Args> {}
        impl<S, Args> Sealed for super::FallibleSelf<S, Args> {}
        impl<S, Args> Sealed for super::InfallibleSelf<S, Args> {}
    }

    /// Defines the common [`Receiver`][`Self::Receiver`] type of [`ImmutableSelf`]
    /// and [`MutableSelf`].
    pub trait Receiver {
        /// The type that receives the method call.
        type Receiver;
    }

    /// Marker for a method that has an immutable `self` (`&self`).
    pub struct ImmutableSelf<S>(PhantomData<S>);
    /// Marker for a method that has a mutable `self` (`&mut self`).
    pub struct MutableSelf<S>(PhantomData<S>);

    impl<S> Receiver for ImmutableSelf<S> {
        type Receiver = S;
    }

    impl<S> Receiver for MutableSelf<S> {
        type Receiver = S;
    }

    /// Marker trait for all functions that accept a `self` reference as the first parameter.
    ///
    /// See also [`Bare`].
    pub trait Method<S>: method::Sealed
    where
        S: ?Sized,
    {
    }

    impl<'s, Args> Method<super::RawSelf<'s>> for FallibleRawSelf<Args> {}
    impl<'s, Args> Method<super::RawSelf<'s>> for InfallibleRawSelf<Args> {}
    impl<S, Args> Method<S::Receiver> for FallibleSelf<S, Args> where S: Receiver {}
    impl<S, Args> Method<S::Receiver> for InfallibleSelf<S, Args> where S: Receiver {}
}

macro_rules! impl_non_varargs {
    // This macro is ultra-flexible, in that it could support any calling convention.
    // Not that it needs to.
    // See the other arm for usage.
    (
        @for $variant:tt, $($variant_args:ty),+;
        $($genericdef:tt),+;
        where
            $($genericty:ty : $bound:path),+;
        $params:tt;
        $(base_parameter_count $base_parameter_count:tt;)?
        $(let $this:tt = $setup:tt;)?
        |$arguments:tt| $call_args:tt;
        $ret:ty;
        |$gc:tt, $result:tt| $map:expr;
        $($types:tt),*
    ) => {
        /// Implementation for a static number of arguments.
        impl<
            $($genericdef,)+
            Fun,
            $($types),*
        > $crate::ForeignFunction<$crate::ffvariants::$variant<$($variant_args,)+>> for Fun // :)
        where
            $($genericty: $bound + 'static,)+
            Fun: FnMut $params -> $ret + 'static,
            $($types: TryFromValue + 'static,)*
        {
            fn parameter_count() -> Option<u16> {
                const N: u16 = {
                    #[allow(unused)]
                    let n = 0;
                    $(let n = $base_parameter_count;)?
                    $(
                        // To force Rust into expanding $var into a sequence of additions, we create an
                        // unused variable to drive the expansion. Constant evaluation will ensure n is
                        // a constant integer by the time compilation is finished.
                        #[allow(unused, non_snake_case)]
                        let $types = 0;
                        let n = n + 1;
                    )*
                    n
                };
                Some(N)
            }

            fn into_raw_foreign_function(mut self) -> RawForeignFunction {
                #[allow(unused_imports)]
                use $crate::MicaLanguageResultExt;
                create_rawff(move |$gc, args| {
                    let $arguments = Arguments::new(args);
                    let _n = 0;
                    // Similar to `parameter_count`, we use $types as a driver for a sequential counter.
                    // This time though we actually use the variables.
                    $(
                        #[allow(non_snake_case)]
                        let $types = $arguments.get(_n).mica_language()?;
                        #[allow(unused)]
                        let _n = _n + 1;
                    )*
                    {
                        $(let ($this, _guard) = $setup;)?
                        let $result = self $call_args;
                        $map
                    }
                })
            }
        }
    };

    ($($types:tt),*) => {
        // Help wanted!
        // Uuuuuuuuuhhhh
        // ummmmmmmmmmmmmmmm
        // uhhhhhhh...
        // oh no.

        // Behold, ye who enter here.
        // For this land is an absolute fucking mess and thou shall not add more calling conventions.
        // Well at this point it's more about how much of a repetitive mess this has become.
        // LOOK AT IT.
        // It makes me wanna puke.

        impl_non_varargs!(
            // First we define the trait that is to be implemented.
            @for Fallible, ($($types,)*);
            // Then the generic types/bounds of the `impl`.
            Ret, Err;
            where
                Ret: Into<$crate::Value>,
                Err: std::error::Error;
            // Then the arguments of the `FnMut`.
            ($($types),*);
            // Then the caller that expands argument to the function call.
            |_arguments| ($($types,)*);
            // Then the return type of this calling convention.
            Result<Ret, Err>;
            // Then the "mapper" "function" that maps the function's result into the raw
            // calling convention.
            |gc, result| result
                .map(|value| value.into().to_raw(gc))
                .map_err(|error| LanguageErrorKind::User(Box::new(error)));
            // Lastly we pass on the generic parameters entered into the macro.
            $($types),*
        );
        impl_non_varargs!(
            @for Infallible, ($($types,)*);
            Ret;
            where Ret: Into<$crate::Value>;
            ($($types),*);
            |_arguments| ($($types,)*);
            Ret;
            |gc, value| Ok(value.into().to_raw(gc));
            $($types),*
        );

        impl_non_varargs!(
            @for FallibleRawSelf, ($($types,)*);
            Ret, Err;
            where
                Ret: Into<$crate::Value>,
                Err: std::error::Error;
            ($crate::RawSelf, $($types),*);
            // base_parameter_count needs to be provided for methods that accept `self`, because the
            // `self` argument is not implicit in the parameter count
            base_parameter_count 1;
            |arguments| (arguments.raw_self().into(), $($types,)*);
            Result<Ret, Err>;
            |gc, result| result
                .map(|value| value.into().to_raw(gc))
                .map_err(|error| LanguageErrorKind::User(Box::new(error)));
            $($types),*
        );
        impl_non_varargs!(
            @for InfallibleRawSelf, ($($types,)*);
            Ret;
            where Ret: Into<$crate::Value>;
            ($crate::RawSelf, $($types),*);
            base_parameter_count 1;
            |arguments| (arguments.raw_self().into(), $($types,)*);
            Ret;
            |gc, value| Ok(value.into().to_raw(gc));
            $($types),*
        );

        impl_non_varargs!(
            @for FallibleSelf, $crate::ffvariants::ImmutableSelf<S>, (&S, $($types,)*);
            S, Ret, Err;
            where
                Ret: Into<$crate::Value>,
                Err: std::error::Error,
                S: $crate::SelfFromRawValue;
            (&S, $($types),*);
            base_parameter_count 1;
            let this = {
                unsafe {
                    $crate::SelfFromRawValue::self_from_raw_value(arguments.raw_self()).mica_language()?
                }
            };
            |arguments| (this, $($types,)*);
            Result<Ret, Err>;
            |gc, result| result
                .map(|value| value.into().to_raw(gc))
                .map_err(|error| LanguageErrorKind::User(Box::new(error)));
            $($types),*
        );
        impl_non_varargs!(
            @for FallibleSelf, $crate::ffvariants::MutableSelf<S>, (&mut S, $($types,)*);
            S, Ret, Err;
            where
                Ret: Into<$crate::Value>,
                Err: std::error::Error,
                S: $crate::MutSelfFromRawValue;
            (&mut S, $($types),*);
            base_parameter_count 1;
            let this = {
                unsafe { S::mut_self_from_raw_value(arguments.raw_self()) }.mica_language()?
            };
            |arguments| (this, $($types,)*);
            Result<Ret, Err>;
            |gc, result| result
                .map(|value| value.into().to_raw(gc))
                .map_err(|error| LanguageErrorKind::User(Box::new(error)));
            $($types),*
        );

        impl_non_varargs!(
            @for InfallibleSelf, $crate::ffvariants::ImmutableSelf<S>, (&S, $($types,)*);
            S, Ret;
            where
                Ret: Into<$crate::Value>,
                S: $crate::SelfFromRawValue;
            (&S, $($types),*);
            base_parameter_count 1;
            let this = {
                unsafe {
                    $crate::SelfFromRawValue::self_from_raw_value(arguments.raw_self()).mica_language()?
                }
            };
            |arguments| (this, $($types,)*);
            Ret;
            |gc, value| Ok(value.into().to_raw(gc));
            $($types),*
        );
        impl_non_varargs!(
            @for InfallibleSelf, $crate::ffvariants::MutableSelf<S>, (&mut S, $($types,)*);
            S, Ret;
            where
                Ret: Into<$crate::Value>,
                S: $crate::MutSelfFromRawValue;
            (&mut S, $($types),*);
            base_parameter_count 1;
            let this = {
                unsafe { S::mut_self_from_raw_value(arguments.raw_self()) }.mica_language()?
            };
            |arguments| (this, $($types,)*);
            Ret;
            |gc, value| Ok(value.into().to_raw(gc));
            $($types),*
        );
    };
}

// We only have support for up to 8 arguments, because anyone reasonable with their code shouldn't
// use more anyways. Every argument we add results in a lot of code being generated, which slows
// down compilation. See the macro expansion if you don't believe me.
// -----
// Rust-analyzer isn't too happy about this macro declaring variables with non-snake_case names.
// In reality it doesn't hurt anything and actually makes code _more_ readable and maintainable
// by reducing the amount of arguments we have to pass to the macro.
impl_non_varargs!();
impl_non_varargs!(A);
impl_non_varargs!(A, B);
impl_non_varargs!(A, B, C);
impl_non_varargs!(A, B, C, D);
impl_non_varargs!(A, B, C, D, E);
impl_non_varargs!(A, B, C, D, E, F);
impl_non_varargs!(A, B, C, D, E, F, G);
impl_non_varargs!(A, B, C, D, E, F, G, H);

/// Implementation for a variable number of arguments.
impl<Ret, Err, F> ForeignFunction<ffvariants::VarargsFallible> for F
where
    Ret: Into<Value> + 'static,
    Err: std::error::Error + 'static,
    F: FnMut(Arguments) -> Result<Ret, Err> + 'static,
{
    fn parameter_count() -> Option<u16> {
        None
    }

    fn into_raw_foreign_function(mut self) -> RawForeignFunction {
        create_rawff(move |gc, args| {
            self(Arguments::new(args))
                .map(|value| value.into().to_raw(gc))
                .map_err(|error| LanguageErrorKind::User(Box::new(error)))
        })
    }
}

/// Implementation for a variable number of arguments.
impl<Ret, F> ForeignFunction<ffvariants::VarargsInfallible> for F
where
    Ret: Into<Value> + 'static,
    F: FnMut(Arguments) -> Ret + 'static,
{
    fn parameter_count() -> Option<u16> {
        None
    }

    fn into_raw_foreign_function(mut self) -> RawForeignFunction {
        create_rawff(move |gc, args| Ok(self(Arguments::new(args)).into().to_raw(gc)))
    }
}
