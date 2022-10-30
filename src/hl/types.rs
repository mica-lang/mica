use std::{any::Any, fmt, marker::PhantomData, rc::Rc};

use crate::{
    builtin_traits::{BuiltinTrait, BuiltinTraitFunction},
    ffvariants,
    hl::userdata::Type,
    ll::{
        bytecode::{
            BuiltinTraits, DispatchTable, Environment, Function, FunctionKind, MethodSignature,
        },
        gc::{Gc, Memory},
        value::{self, Closure},
    },
    Error, ForeignFunction, FunctionParameterCount, MethodParameterCount, Object,
    ObjectConstructor, RawForeignFunction, Value,
};

type Constructor<T> = Box<dyn FnOnce(Rc<dyn ObjectConstructor<T>>) -> RawForeignFunction>;

struct UnresolvedMethodSignature {
    name: Rc<str>,
    parameter_count: MethodParameterCount,
    builtin_trait: BuiltinTrait,
}

impl UnresolvedMethodSignature {
    fn resolve(self, builtin_traits: &BuiltinTraits) -> MethodSignature {
        MethodSignature {
            name: self.name,
            parameter_count: self.parameter_count,
            trait_id: match self.builtin_trait {
                BuiltinTrait::None => None,
                BuiltinTrait::Iterator => Some(builtin_traits.iterator),
            },
        }
    }
}

/// A descriptor for a dispatch table. Defines which methods are available on the table, as well
/// as their implementations.
#[derive(Default)]
pub(crate) struct DispatchTableDescriptor {
    methods: Vec<(UnresolvedMethodSignature, FunctionKind)>,
}

impl DispatchTableDescriptor {
    fn add_function_to_dtable(
        env: &mut Environment,
        gc: &mut Memory,
        dtable: &mut DispatchTable,
        builtin_traits: &BuiltinTraits,
        signature: UnresolvedMethodSignature,
        f: FunctionKind,
    ) -> Result<(), Error> {
        let name = Rc::from(format!("{}.{}", &dtable.pretty_name, signature.name));
        let function_id = env
            .create_function(Function {
                name: Rc::clone(&name),
                parameter_count: FunctionParameterCount::Fixed(u16::from(
                    signature.parameter_count.to_count_without_self(),
                )),
                kind: f,
                hidden_in_stack_traces: false,
            })
            .map_err(|_| Error::TooManyFunctions)?;
        let signature = signature.resolve(builtin_traits);
        let index =
            env.get_or_create_method_index(&signature).map_err(|_| Error::TooManyMethods)?;
        dtable.set_method(index, gc.allocate(Closure { name, function_id, captures: Vec::new() }));
        Ok(())
    }

    /// Builds a dispatch table from this descriptor.
    pub(crate) fn build_dtable(
        self,
        mut dtable: DispatchTable,
        env: &mut Environment,
        gc: &mut Memory,
        builtin_traits: &BuiltinTraits,
    ) -> Result<DispatchTable, Error> {
        for (signature, f) in self.methods {
            Self::add_function_to_dtable(env, gc, &mut dtable, builtin_traits, signature, f)?;
        }
        Ok(dtable)
    }
}

/// A builder that allows for binding APIs with user-defined types.
///
/// By default, the Mica VM does not know anything and cannot interact with Rust types. This API,
/// in conjunction with [`Engine::add_type`][crate::Engine::add_type], serves as an extension point
/// to let Mica programs interact with Rust data.
///
/// # Limitations
///
/// Currently the ability to use custom types in Mica is quite limited. Here's a list of all the
/// limitations:
/// - Your type can only be the `self` parameter of a function, even if it's [`Clone`].
/// - By extension, you cannot take custom types as function arguments by reference.
/// - Your type must be constructed from a special constructor function that's only available as
///   part of [`add_constructor`][TypeBuilder::add_constructor]'s callback.
///
/// Eventually, most of these limitations will be lifted because they greatly limit the potential of
/// what can be done with Mica, and all are solvable in a reasonable manner. But the current version
/// unfortunately has its limits and you will have to work around them (or use a more mature
/// language. Not that there are many choices.)
pub struct TypeBuilder<T>
where
    T: ?Sized,
{
    type_name: Rc<str>,
    type_dtable: DispatchTableDescriptor,
    instance_dtable: DispatchTableDescriptor,
    constructors: Vec<(UnresolvedMethodSignature, Constructor<T>)>,
    _data: PhantomData<T>,
}

impl<T> TypeBuilder<T>
where
    T: ?Sized,
{
    /// Creates a new [`TypeBuilder`].
    ///
    /// The `type_name` is used for referring to the type inside scripts and should reflect the Rust
    /// type name. For generic types, it's best to define a concrete [type alias][type] to make the
    /// binding code a little bit more self-documenting.
    ///
    ///   [type]: https://doc.rust-lang.org/stable/std/keyword.type.html
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, TypeBuilder, UserData};
    ///
    /// struct Empty;
    /// impl UserData for Empty {}
    ///
    /// // A type builder is useless on its own, so we need to create an engine first.
    /// let mut engine = Engine::new();
    /// engine.add_type(TypeBuilder::<Empty>::new("Empty"))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(type_name: impl Into<Rc<str>>) -> Self
    where
        T: Any,
    {
        let type_name = type_name.into();
        Self {
            type_dtable: Default::default(),
            instance_dtable: Default::default(),
            constructors: Vec::new(),
            type_name,
            _data: PhantomData,
        }
    }

    /// Internal converter for use with `BareExactArgs` parameter counts.
    fn function_to_method_parameter_count(count: FunctionParameterCount) -> MethodParameterCount {
        MethodParameterCount::from_count_without_self(
            count.to_fixed().expect("BareExactArgs functions are never varargs"),
        )
        .expect("generated ForeignFunction variants only support up to 8 arguments") // Thus, overflow is impossible.
    }

    /// Adds a static function to the struct.
    ///
    /// The function must follow the "bare" calling convention, in that it doesn't accept a
    /// reference to `T` as its first parameter.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, TypeBuilder, UserData};
    ///
    /// struct Constants;
    /// impl UserData for Constants {}
    ///
    /// let mut engine = Engine::new();
    /// engine.add_type(
    ///     TypeBuilder::<Constants>::new("Constants")
    ///         .add_static("the_meaning_of_life_universe_and_everything", || 42_i32),
    /// )?;
    ///
    /// let i: i32 = engine
    ///     .start("constant.mi", "Constants.the_meaning_of_life_universe_and_everything")?
    ///     .trampoline()?;
    /// assert_eq!(i, 42);
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_static<F, V>(self, name: &str, f: F) -> Self
    where
        V: ffvariants::BareExactArgs,
        F: ForeignFunction<V, ParameterCount = FunctionParameterCount>,
    {
        self.add_raw_static(
            name,
            Self::function_to_method_parameter_count(F::PARAMETER_COUNT),
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        )
    }

    /// Adds a constructor function to the struct.
    ///
    /// A constructor is a static function responsible for creating instances of a type. The
    /// function passed to this one must return another function that actually constructs the
    /// object.
    ///
    /// # Examples
    /// See [`TypeBuilder::add_function`].
    pub fn add_constructor<C, F, V>(self, name: &str, f: C) -> Self
    where
        V: ffvariants::BareExactArgs,
        C: FnOnce(Rc<dyn ObjectConstructor<T>>) -> F,
        C: 'static,
        F: ForeignFunction<V, ParameterCount = FunctionParameterCount>,
    {
        self.add_raw_constructor(
            name,
            Self::function_to_method_parameter_count(F::PARAMETER_COUNT),
            Box::new(|ctor| f(ctor).into_raw_foreign_function()),
        )
    }

    /// Adds an instance function to the struct.
    ///
    /// The function must follow the "method" calling convention, in that it accepts `&T` or
    /// `&mut T` as its first parameter.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, TypeBuilder, UserData};
    ///
    /// struct Counter {
    ///     value: i32,
    /// }
    ///
    /// impl UserData for Counter {}
    ///
    /// impl Counter {
    ///     fn increment(&mut self) {
    ///         self.value += 1;
    ///     }
    ///
    ///     fn value(&self) -> i32 {
    ///         self.value
    ///     }
    /// }
    ///
    /// let mut engine = Engine::new();
    /// engine.add_type(
    ///     TypeBuilder::<Counter>::new("Counter")
    ///         .add_constructor("new", |ctor| move || {
    ///             ctor.construct(Counter { value: 0 })
    ///         })
    ///         .add_function("increment", Counter::increment)
    ///         .add_function("value", Counter::value),
    /// )?;
    ///
    /// let i: i32 = engine
    ///     .start(
    ///         "counter.mi",
    ///         r#" count = Counter.new()
    ///             count.increment()
    ///             count.increment()
    ///             count.value "#
    ///     )?
    ///     .trampoline()?;
    /// assert_eq!(i, 2);
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_function<F, V>(self, name: &str, f: F) -> Self
    where
        V: ffvariants::Method<T>,
        F: ForeignFunction<V, ParameterCount = MethodParameterCount>,
    {
        self.add_raw_function(
            name,
            F::PARAMETER_COUNT,
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        )
    }

    /// Adds a function that's part of a built-in trait implementation.
    ///
    /// The function must have a signature that's compatible with the built-in trait in question.
    /// See the [`builtin_traits`][crate::builtin_traits] module for more information on each trait,
    /// its methods, and their signatures.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{builtin_traits::iterator, Engine, TypeBuilder, UserData};
    ///
    /// struct Count10 {
    ///     i: i32,
    /// }
    ///
    /// impl UserData for Count10 {}
    ///
    /// impl Count10 {
    ///     fn has_next(&self) -> bool {
    ///         self.i < 10
    ///     }
    ///
    ///     fn next(&mut self) {
    ///         self.i += 1;
    ///     }
    /// }
    ///
    /// let mut engine = Engine::new();
    /// engine.add_type(
    ///     TypeBuilder::<Count10>::new("Count10")
    ///         .add_constructor("new", |ctor| move || {
    ///             ctor.construct(Count10 { i: 1 })
    ///         })
    ///         .add_builtin_trait_function(iterator::HasNext, Count10::has_next)
    ///         .add_builtin_trait_function(iterator::Next, Count10::next),
    /// )?;
    ///
    /// let i: i32 = engine
    ///     .start(
    ///         "counter.mi",
    ///         r#"
    ///             i = 1
    ///             for _ in Count10.new() do
    ///                 i = i * 2
    ///             end
    ///             i
    ///         "#
    ///     )?
    ///     .trampoline()?;
    /// assert_eq!(i, 512);
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_builtin_trait_function<S, B, F>(mut self, which: B, f: F) -> Self
    where
        B: BuiltinTraitFunction<S>,
        F: ForeignFunction<S, ParameterCount = MethodParameterCount>,
    {
        self.instance_dtable.methods.push((
            UnresolvedMethodSignature {
                name: Rc::from(B::NAME),
                parameter_count: F::PARAMETER_COUNT,
                builtin_trait: which.owning_trait(),
            },
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        ));
        self
    }

    /// Adds a _raw_ instance function to the type.
    ///
    /// You should generally prefer [`add_function`][`Self::add_function`] instead of this.
    ///
    /// `parameter_count` should reflect the parameter count of the function. Method calls resolve
    /// differently from function calls, because they match the parameter count exactly - it is
    /// impossible to call the method `my_method/2` with three parameters. Thus, you can expect
    /// the `arguments` array inside of foreign functions to always have `parameter_count` elements.
    pub fn add_raw_function(
        mut self,
        name: &str,
        parameter_count: MethodParameterCount,
        f: FunctionKind,
    ) -> Self {
        self.instance_dtable.methods.push((
            UnresolvedMethodSignature {
                name: Rc::from(name),
                parameter_count,
                builtin_trait: BuiltinTrait::None,
            },
            f,
        ));
        self
    }

    /// Adds a _raw_ static function to the type.
    ///
    /// You should generally prefer [`add_static`][`Self::add_static`] instead of this.
    ///
    /// `parameter_count` should reflect the parameter count of the function. Method calls resolve
    /// differently from function calls, because they match the parameter count exactly - it is
    /// impossible to call the method `my_method/2` with three parameters. Thus, you can expect
    /// the `arguments` array inside of foreign functions to always have `parameter_count` elements.
    pub fn add_raw_static(
        mut self,
        name: &str,
        parameter_count: MethodParameterCount,
        f: FunctionKind,
    ) -> Self {
        self.type_dtable.methods.push((
            UnresolvedMethodSignature {
                name: Rc::from(name),
                parameter_count,
                builtin_trait: BuiltinTrait::None,
            },
            f,
        ));
        self
    }

    /// Adds a _raw_ constructor to the type.
    ///
    /// You should generally prefer [`add_constructor`][`Self::add_constructor`] instead of this.
    ///
    /// `parameter_count` should reflect the parameter count of the function. Method calls resolve
    /// differently from function calls, because they match the parameter count exactly - it is
    /// impossible to call the method `my_method/2` with three parameters. Thus, you can expect
    /// the `arguments` array inside of foreign functions to always have `parameter_count` elements.
    pub fn add_raw_constructor(
        mut self,
        name: &str,
        parameter_count: MethodParameterCount,
        f: Constructor<T>,
    ) -> Self {
        self.constructors.push((
            UnresolvedMethodSignature {
                name: Rc::from(name),
                parameter_count,
                builtin_trait: BuiltinTrait::None,
            },
            f,
        ));
        self
    }

    /// Builds the struct builder into its type dtable and instance dtable, respectively.
    pub(crate) fn build(
        self,
        env: &mut Environment,
        gc: &mut Memory,
        builtin_traits: &BuiltinTraits,
    ) -> Result<BuiltType<T>, Error>
    where
        T: Any + Sized,
    {
        let mut type_dtable = self.type_dtable.build_dtable(
            DispatchTable::new_for_type(Rc::clone(&self.type_name)),
            env,
            gc,
            builtin_traits,
        )?;

        let instance_dtable = self.instance_dtable.build_dtable(
            DispatchTable::new_for_instance(Rc::clone(&self.type_name)),
            env,
            gc,
            builtin_traits,
        )?;
        let instance_dtable = Gc::new(instance_dtable);
        env.add_user_dtable::<T>(Gc::clone(&instance_dtable));

        // The type dtable also contains constructors, so build those while we're at it.
        // We implement a helper here that fulfills the ObjectConstructor trait.

        struct Instancer<T>
        where
            T: Sized,
        {
            instance_dtable: Gc<DispatchTable>,
            _data: PhantomData<T>,
        }

        impl<T> ObjectConstructor<T> for Instancer<T> {
            fn construct(&self, instance: T) -> Object<T> {
                Object::new(Gc::as_raw(&self.instance_dtable), instance)
            }
        }

        let instancer: Rc<dyn ObjectConstructor<T>> =
            Rc::new(Instancer { instance_dtable: Gc::clone(&instance_dtable), _data: PhantomData });
        for (signature, constructor) in self.constructors {
            let f = constructor(Rc::clone(&instancer));
            DispatchTableDescriptor::add_function_to_dtable(
                env,
                gc,
                &mut type_dtable,
                builtin_traits,
                signature,
                FunctionKind::Foreign(f),
            )?;
        }

        type_dtable.instance = Some(Gc::as_raw(&instance_dtable));

        let type_dtable = Gc::new(type_dtable);
        Ok(BuiltType {
            type_dtable,
            instance_dtable,
            type_name: self.type_name,
            _data: PhantomData,
        })
    }
}

impl<T> fmt::Debug for TypeBuilder<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeBuilder").finish_non_exhaustive()
    }
}

/// Dispatch tables for a finished type.
pub(crate) struct BuiltType<T>
where
    T: ?Sized,
{
    pub(crate) type_name: Rc<str>,
    pub(crate) type_dtable: Gc<DispatchTable>,
    pub(crate) instance_dtable: Gc<DispatchTable>,
    _data: PhantomData<T>,
}

impl<T> BuiltType<T> {
    /// Makes a `Type<T>` user data value from the built type.
    pub(crate) fn make_type(&self, gc: &mut Memory) -> Value
    where
        T: Any,
    {
        gc.manage(&self.type_dtable);
        gc.manage(&self.instance_dtable);
        let user_data: Box<dyn value::UserData> =
            Box::new(Type::<T>::new(Gc::clone(&self.type_dtable)));
        Value::UserData(Gc::new(user_data))
    }
}
