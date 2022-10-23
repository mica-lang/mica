use std::{any::Any, marker::PhantomData, rc::Rc};

use mica_language::{
    bytecode::{
        BuiltinTraits, DispatchTable, Environment, Function, FunctionKind, FunctionSignature,
    },
    gc::{Gc, Memory},
    value::{self, Closure},
};

use crate::{
    builtin_traits::{BuiltinTrait, BuiltinTraitFunction},
    ffvariants,
    userdata::Type,
    Error, ForeignFunction, Object, ObjectConstructor, RawForeignFunction, Value,
};

type Constructor<T> = Box<dyn FnOnce(Rc<dyn ObjectConstructor<T>>) -> RawForeignFunction>;

struct UnresolvedFunctionSignature {
    name: Rc<str>,
    arity: Option<u16>,
    builtin_trait: BuiltinTrait,
}

impl UnresolvedFunctionSignature {
    fn resolve(self, builtin_traits: &BuiltinTraits) -> FunctionSignature {
        FunctionSignature {
            name: self.name,
            arity: self.arity,
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
    methods: Vec<(UnresolvedFunctionSignature, FunctionKind)>,
}

impl DispatchTableDescriptor {
    fn add_function_to_dtable(
        env: &mut Environment,
        gc: &mut Memory,
        dtable: &mut DispatchTable,
        builtin_traits: &BuiltinTraits,
        signature: UnresolvedFunctionSignature,
        f: FunctionKind,
    ) -> Result<(), Error> {
        let name = Rc::from(format!("{}.{}", &dtable.pretty_name, signature.name));
        let function_id = env
            .create_function(Function {
                name: Rc::clone(&name),
                parameter_count: signature.arity,
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
pub struct TypeBuilder<T>
where
    T: ?Sized,
{
    type_name: Rc<str>,
    type_dtable: DispatchTableDescriptor,
    instance_dtable: DispatchTableDescriptor,
    constructors: Vec<(UnresolvedFunctionSignature, Constructor<T>)>,
    _data: PhantomData<T>,
}

impl<T> TypeBuilder<T>
where
    T: ?Sized,
{
    /// Creates a new `TypeBuilder`.
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

    /// Adds a _raw_ instance function to the type.
    ///
    /// You should generally prefer [`add_function`][`Self::add_function`] instead of this.
    ///
    /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
    /// function accepts a variable number of arguments. Note that _unlike with bare raw functions_
    /// there can be two functions with the same name defined on a type, as long as they have
    /// different arities. Functions with specific arities take priority over varargs.
    ///
    /// Note that this function _consumes_ the builder; this is because calls to functions that add
    /// into the type are meant to be chained together in one expression.
    pub fn add_raw_function(
        mut self,
        name: &str,
        parameter_count: Option<u16>,
        f: FunctionKind,
    ) -> Self {
        self.instance_dtable.methods.push((
            UnresolvedFunctionSignature {
                name: Rc::from(name),
                arity: parameter_count,
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
    /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
    /// function accepts a variable number of arguments. Note that _unlike with bare raw functions_
    /// there can be two functions with the same name defined on a type, as long as they have
    /// different arities. Functions with specific arities take priority over varargs.
    ///
    /// Note that this function _consumes_ the builder; this is because calls to functions that add
    /// into the type are meant to be chained together in one expression.
    pub fn add_raw_static(
        mut self,
        name: &str,
        parameter_count: Option<u16>,
        f: FunctionKind,
    ) -> Self {
        self.type_dtable.methods.push((
            UnresolvedFunctionSignature {
                name: Rc::from(name),
                arity: parameter_count,
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
    /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
    /// function accepts a variable number of arguments. Note that _unlike with bare raw functions_
    /// there can be two functions with the same name defined on a type, as long as they have
    /// different arities. Functions with specific arities take priority over varargs.
    ///
    /// Note that this function _consumes_ the builder; this is because calls to functions that add
    /// into the type are meant to be chained together in one expression.
    pub fn add_raw_constructor(
        mut self,
        name: &str,
        parameter_count: Option<u16>,
        f: Constructor<T>,
    ) -> Self {
        self.constructors.push((
            UnresolvedFunctionSignature {
                name: Rc::from(name),
                arity: parameter_count,
                builtin_trait: BuiltinTrait::None,
            },
            f,
        ));
        self
    }

    /// Adds an instance function to the struct.
    ///
    /// The function must follow the "method" calling convention, in that it accepts `&T` or
    /// `&mut T` as its first parameter.
    pub fn add_function<F, V>(self, name: &str, f: F) -> Self
    where
        V: ffvariants::Method<T>,
        F: ForeignFunction<V>,
    {
        self.add_raw_function(
            name,
            F::parameter_count(),
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        )
    }

    /// Adds a function that's part of a built-in trait implementation.
    ///
    /// The function must have a signature that's compatible with the built-in trait in question.
    pub fn add_builtin_trait_function<S, B, F>(mut self, which: B, f: F) -> Self
    where
        B: BuiltinTraitFunction<S>,
        F: ForeignFunction<S>,
    {
        self.instance_dtable.methods.push((
            UnresolvedFunctionSignature {
                name: Rc::from(B::NAME),
                arity: F::parameter_count(),
                builtin_trait: which.owning_trait(),
            },
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        ));
        self
    }

    /// Adds a static function to the struct.
    ///
    /// The function must follow the "bare" calling convention, in that it doesn't accept a
    /// reference to `T` as its first parameter.
    pub fn add_static<F, V>(self, name: &str, f: F) -> Self
    where
        V: ffvariants::Bare,
        F: ForeignFunction<V>,
    {
        self.add_raw_static(
            name,
            F::parameter_count().map(|x| {
                // Add 1 for the static receiver, which isn't counted into the bare function's
                // signature.
                x + 1
            }),
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        )
    }

    /// Adds a constructor to the type.
    ///
    /// A constructor is a static function responsible for creating instances of a type. The
    /// function passed to this one must return another function that actually constructs the
    /// object.
    ///
    /// Constructors use the "bare" calling convention, in that they don't accept a `self`
    /// parameter.
    pub fn add_constructor<F, G, V>(self, name: &str, f: F) -> Self
    where
        V: ffvariants::Bare,
        F: FnOnce(Rc<dyn ObjectConstructor<T>>) -> G,
        F: 'static,
        G: ForeignFunction<V>,
    {
        self.add_raw_constructor(
            name,
            G::parameter_count().map(|x| x + 1),
            Box::new(|ctor| f(ctor).into_raw_foreign_function()),
        )
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
            fn construct(&self, instance: T) -> crate::Object<T> {
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

        // Build the instance dtable.
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
