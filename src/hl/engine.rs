use std::{any::Any, collections::HashMap, fmt, fmt::Debug, ops::Deref, rc::Rc};

/// The implementation of a raw foreign function.
pub use crate::ll::bytecode::ForeignFunction as RawForeignFunction;
/// The kind of a raw function.
pub use crate::ll::bytecode::FunctionKind as RawFunctionKind;
use crate::{
    corelib, create_trait_value, ffvariants,
    ll::{
        ast::DumpAst,
        bytecode,
        bytecode::{
            BuiltinDispatchTableGenerator, BuiltinDispatchTables, BuiltinTraits, Chunk,
            DispatchTable, Environment, Function, FunctionKind, GlobalIndex, Library, MethodIndex,
            Opcode, Opr24,
        },
        codegen::{self, CodeGenerator},
        gc::{Gc, Memory},
        lexer::Lexer,
        parser::Parser,
        value::{Closure, RawValue},
        vm::{self, Globals},
    },
    BuiltType, CoreLibrary, Error, Fiber, ForeignFunction, FunctionParameterCount, IntoValue,
    MethodParameterCount, MicaResultExt, TraitBuilder, TryFromValue, TypeBuilder, UserData, Value,
};

/// Options for debugging the language implementation.
#[derive(Debug, Clone, Copy, Default)]
pub struct DebugOptions {
    /// Set to `true` to print the AST to stdout after parsing.
    pub dump_ast: bool,
    /// Set to `true` to print the bytecode to stdout after successful compilation.
    pub dump_bytecode: bool,
}

/// **Start here!** An execution engine. Contains information about things like globals, registered
/// types, etc.
#[derive(Debug)]
pub struct Engine {
    pub(crate) env: Environment,
    pub(crate) library: Library,
    pub(crate) globals: Globals,
    // This field is needed to keep all builtin dispatch tables alive for longer than `gc`.
    pub(crate) gc: Memory,
    debug_options: DebugOptions,
}

impl Engine {
    /// Creates a new engine using the [default core library][corelib].
    ///
    /// Although [`Engine::with_debug_options`] and likewise [`Engine::with_corelib`] can panic
    /// if the core library is much too big, the official core library is not big enough to cause
    /// this.
    ///
    /// # Examples
    /// ```
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::new();
    /// ```
    pub fn new() -> Self {
        Self::with_corelib(corelib::Lib)
    }

    /// Creates a new engine with an alternative core library.
    ///
    /// # Panics
    /// Constructing the engine can panic if the core library defines too many globals, functions,
    /// methods, or the like. See [`Engine::with_debug_options`] for more remarks.
    ///
    /// # Examples
    /// ```
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::with_corelib(mica::corelib::Lib);
    /// ```
    pub fn with_corelib<L>(corelib: L) -> Self
    where
        L: CoreLibrary,
    {
        Self::with_debug_options(corelib, Default::default())
    }

    /// Creates a new engine with specific debug options.
    ///
    /// [`Engine::new`] and [`Engine::with_corelib`] create engines with [`Default`] debug options,
    /// and should generally be preferred unless you're debugging the language's internals.
    ///
    /// # Panics
    /// Constructing the engine can panic if the core library defines too many globals, functions,
    /// methods, or the like. In reality this is only really a problem if you let users control
    /// the amount of methods registered by your core library (which you should never, ever do.)
    ///
    /// # Examples
    /// ```
    /// use mica::{Engine, DebugOptions};
    ///
    /// // Create a loud engine that prints a bunch of debugging information to stdout.
    /// let mut engine = Engine::with_debug_options(mica::corelib::Lib, DebugOptions {
    ///     dump_ast: true,
    ///     dump_bytecode: true,
    /// });
    /// ```
    pub fn with_debug_options<L>(corelib: L, debug_options: DebugOptions) -> Self
    where
        L: CoreLibrary,
    {
        #[derive(Debug)]
        struct DtableGenerator<L> {
            corelib: L,
        }

        impl<L> BuiltinDispatchTableGenerator for DtableGenerator<L>
        where
            L: CoreLibrary + Debug,
        {
            fn generate_tuple(
                &self,
                env: &mut Environment,
                gc: &mut Memory,
                builtin_traits: &BuiltinTraits,
                size: usize,
            ) -> Gc<DispatchTable> {
                self.corelib
                    .define_tuple(size, TypeBuilder::new(format!("Tuple({size})")))
                    .build(env, gc, builtin_traits)
                    .expect("corelib declares too many methods")
                    .instance_dtable
            }

            fn generate_record(
                &self,
                env: &mut Environment,
                gc: &mut Memory,
                builtin_traits: &BuiltinTraits,
                identifier: &str,
            ) -> Gc<DispatchTable> {
                let fields = identifier.split('+').enumerate().map(|(index, name)| (name, index));
                let type_name = if identifier.len() == 0 {
                    String::from("Record{}")
                } else {
                    format!("Record{{{}}}", identifier.replace("+", ", "))
                };
                self.corelib
                    .define_record(fields, TypeBuilder::new(type_name))
                    .build(env, gc, builtin_traits)
                    .expect("corelib declares too many methods")
                    .instance_dtable
            }
        }

        let mut gc = Memory::new();
        let mut env = Environment::new();

        let builtin_traits = BuiltinTraits::register_in(&mut env);

        macro_rules! get_dtables {
            ($type_name:tt, $define:tt) => {{
                let tb = TypeBuilder::new($type_name);
                let tb = corelib.$define(tb);
                tb.build(&mut env, &mut gc, &builtin_traits)
                    .expect("corelib declares too many methods")
            }};
        }
        let nil = get_dtables!("Nil", define_nil);
        let boolean = get_dtables!("Boolean", define_boolean);
        let number = get_dtables!("Number", define_number);
        let string = get_dtables!("String", define_string);
        let list = get_dtables!("List", define_list);
        let dict = get_dtables!("Dict", define_dict);
        let mut library = Library::new(
            BuiltinDispatchTables {
                nil: Gc::clone(&nil.instance_dtable),
                boolean: Gc::clone(&boolean.instance_dtable),
                number: Gc::clone(&number.instance_dtable),
                string: Gc::clone(&string.instance_dtable),
                function: Gc::new(DispatchTable::new_for_instance("Function")),
                list: Gc::clone(&list.instance_dtable),
                dict: Gc::clone(&dict.instance_dtable),
                tuples: vec![],
                records: vec![],
                records_by_identifier: HashMap::new(),
            },
            Box::new(DtableGenerator { corelib: corelib.clone() }),
            builtin_traits,
        );

        // Pre-generate tuples up to size 8, which is what you can receive or produce automagically
        // using the high-level API. If we don't do that and the VM receives any of those tuples,
        // it'll cause panics whenever the VM tries to touch them.
        for size in 0..=8 {
            library.generate_tuple(&mut env, &mut gc, size);
        }

        let iterator = create_trait_value(&mut env, &mut gc, library.builtin_traits.iterator);

        let mut engine = Self { env, library, globals: Globals::new(), gc, debug_options };
        // Unwrapping here is fine because at this point we haven't got quite that many globals
        // registered to overflow an Opr24.
        engine.set_built_type(&nil).unwrap();
        engine.set_built_type(&boolean).unwrap();
        engine.set_built_type(&number).unwrap();
        engine.set_built_type(&string).unwrap();
        engine.set_built_type(&list).unwrap();
        engine.set("Iterator", iterator).unwrap();

        corelib.load(&mut engine).expect("corelib failed to load (in CoreLibrary::load)");

        engine
    }

    /// Compiles a script without executing it.
    ///
    /// The filename is used for reporting compilation errors and in stack traces.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::new();
    /// let script = engine.compile("example.mi", "2 + 2")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn compile(
        &mut self,
        filename: impl AsRef<str>,
        source: impl Into<String>,
    ) -> Result<Script, Error> {
        let module_name = Rc::from(filename.as_ref());
        let lexer = Lexer::new(Rc::clone(&module_name), source.into());
        let (ast, root_node) = Parser::new(lexer).parse()?;
        if self.debug_options.dump_ast {
            eprintln!("Mica - AST dump:");
            eprintln!("{:?}", DumpAst(&ast, root_node));
        }

        let main_chunk =
            CodeGenerator::new(module_name, &mut self.env, &mut self.library, &mut self.gc)
                .generate(&ast, root_node)?;
        if self.debug_options.dump_bytecode {
            eprintln!("Mica - global environment:");
            eprintln!("{:#?}", self.env);
            eprintln!("Mica - main chunk disassembly:");
            eprintln!("{main_chunk:#?}");
        }

        Ok(Script { engine: self, main_chunk })
    }

    /// Compiles and starts executing a script in a fiber.
    ///
    /// This can be used as a shorthand if you don't intend to reuse the compiled [`Script`].
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::new();
    /// let mut fiber = engine.start("example.mi", "2 + 2")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn start(
        &mut self,
        filename: impl AsRef<str>,
        source: impl Into<String>,
    ) -> Result<Fiber, Error> {
        let script = self.compile(filename, source)?;
        Ok(script.into_fiber())
    }

    /// Calls a function with the given arguments.
    ///
    /// The function is called in a new fiber, and execution is [trampolined][Fiber::trampoline]
    /// until the fiber finishes executing.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, Value};
    ///
    /// let mut engine = Engine::new();
    /// let f: Value = engine.start("example.mi", r#" (func (x) = x + 2) "#)?.trampoline()?;
    /// let result: f64 = engine.call(f, [Value::new(1.0)])?;
    /// assert_eq!(result, 3.0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn call<T>(
        &mut self,
        function: Value,
        arguments: impl IntoIterator<Item = Value>,
    ) -> Result<T, Error>
    where
        T: TryFromValue,
    {
        let stack: Vec<_> =
            Some(function).into_iter().chain(arguments).map(|x| x.to_raw(&mut self.gc)).collect();
        // Having to construct a chunk here isn't the most clean, but it's the simplest way of
        // making the VM perform a function call. It reuses sanity checks such as ensuring
        // `function` can actually be called.
        let mut chunk = Chunk::new(Rc::from("(call)"));
        chunk.emit((
            Opcode::Call,
            // 1 has to be subtracted from the stack length there because the VM itself adds 1 to
            // count in the function argument.
            Opr24::try_from(stack.len() - 1).map_err(|_| Error::TooManyArguments)?,
        ));
        chunk.emit(Opcode::Halt);
        let chunk = Rc::new(chunk);
        let fiber = Fiber { engine: self, inner: vm::Fiber::new(chunk, stack) };
        fiber.trampoline()
    }

    /// Returns the unique ID of a method with a given name and arity.
    ///
    /// Note that there can only exist about 65 thousand unique method signatures. This is usually
    /// not a problem as method names often repeat between types. Also note that unlike functions,
    /// a method can only accept up to 255 arguments. Which, to be quite frankly honest, should be
    /// enough for anyone.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Two identical calls to method_id always return the same ID.
    /// let m_to_string_1 = engine.method_id(("to_string", 0))?;
    /// let m_to_string_2 = engine.method_id(("to_string", 0))?;
    /// let m_pi = engine.method_id(("pi", 0))?;
    /// assert_eq!(m_to_string_1, m_to_string_2);
    /// assert_ne!(m_to_string_1, m_pi);
    /// # Ok(())
    /// # }
    /// ```
    pub fn method_id(&mut self, signature: impl MethodSignature) -> Result<MethodId, Error> {
        signature.to_method_id(&mut self.env)
    }

    /// Calls a method on a receiver with the given arguments.
    ///
    /// Note that if you're calling a method often, it's cheaper to precompute the method signature
    /// into a [`MethodId`] by using the [`method_id`][`Self::method_id`] function, compared to
    /// passing a (name, arity) pair every single time.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, Value};
    ///
    /// let mut engine = Engine::new();
    /// let example = engine
    ///     .start(
    ///         "dog.mi",
    ///         r#" struct Example impl
    ///                 func new() constructor = nil
    ///             end "#
    ///     )?
    ///     .trampoline()?;
    /// let instance: Value = engine.call_method(example, ("new", 0), [])?;
    /// assert!(matches!(instance, Value::Struct(..)));
    /// # Ok(())
    /// # }
    /// ```
    pub fn call_method<T>(
        &mut self,
        receiver: Value,
        signature: impl MethodSignature,
        arguments: impl IntoIterator<Item = Value>,
    ) -> Result<T, Error>
    where
        T: TryFromValue,
    {
        let method_id = signature.to_method_id(&mut self.env)?;
        // Unwrapping here is fine because `to_method_id` ensures that a method with a given ID
        // exists.
        let signature = self.env.get_method_signature(method_id.0).unwrap();
        let stack: Vec<_> =
            Some(receiver).into_iter().chain(arguments).map(|x| x.to_raw(&mut self.gc)).collect();
        let argument_count = MethodParameterCount::from_count_with_self(
            u8::try_from(stack.len()).map_err(|_| Error::TooManyArguments)?,
        );
        if argument_count != signature.parameter_count {
            return Err(Error::ArgumentCount {
                expected: usize::from(signature.parameter_count.to_count_without_self()),
                got: usize::from(argument_count.to_count_without_self()),
            });
        }
        let mut chunk = Chunk::new(Rc::from("(call)"));
        chunk.emit((
            Opcode::CallMethod,
            Opr24::pack((method_id.0.to_u16(), argument_count.to_count_with_self())),
        ));
        chunk.emit(Opcode::Halt);
        let chunk = Rc::new(chunk);
        let fiber = Fiber { engine: self, inner: vm::Fiber::new(chunk, stack) };
        fiber.trampoline()
    }

    /// Creates a new value that is potentially user data.
    ///
    /// User data values need to retrieve type information from the engine upon their creation,
    /// which prevents them from being created by [`Value::new`] which does not possess the same
    /// information.
    ///
    /// Also of note is that because the type information is retrieved _during creation_, so the
    /// type must be [registered][Self::add_type] inside the engine by then; otherwise you'll get
    /// an opaque user data value.
    ///
    /// This needn't be used for user data returned from functions added into the VM, because the
    /// engine automatically does the full conversion under the hood.
    /// <!-- Of course it has to, otherwise the code wouldn't compile. Ha. -->
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, TypeBuilder, UserData, Value};
    ///
    /// struct Cell {
    ///     value: Value,
    /// }
    ///
    /// impl UserData for Cell {}
    ///
    /// let mut engine = Engine::new();
    /// engine.add_type(
    ///     TypeBuilder::<Cell>::new("Cell")
    ///         .add_static("new", |value| Cell { value })
    ///         .add_function("value", |cell: &Cell| cell.value.clone()),
    /// )?;
    ///
    /// // The following will not work, because `Value::new` does not have access to
    /// // Mica type information:
    /// // let cell = Value::new(Cell { value: Value::new(1.0) });
    ///
    /// // The following though, will:
    /// let cell = engine.create_value(Cell { value: Value::new(1.0) });
    /// engine.set("one", cell)?;
    ///
    /// let okay: Result<Value, _> = engine
    ///     .start("okay.mi", "assert(one.value == 1)")?
    ///     .trampoline();
    /// assert!(okay.is_ok());
    /// # Ok(())
    /// # }
    /// ```
    pub fn create_value(&mut self, from: impl IntoValue) -> Value {
        from.into_value_with_engine_state(&self.library, &mut self.gc)
    }

    /// Returns the unique global ID for the global with the given name, or an error if there
    /// are too many globals in scope.
    ///
    /// The maximum amount of globals is about 16 million, so you shouldn't worry too much about
    /// hitting that limit unless you're stress-testing the VM or accepting untrusted input as
    /// globals.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Two identical calls to global_id always return the same ID.
    /// let g_print_1 = engine.global_id("print")?;
    /// let g_print_2 = engine.global_id("print")?;
    /// let g_debug = engine.global_id("debug")?;
    /// assert_eq!(g_print_1, g_print_2);
    /// assert_ne!(g_print_1, g_debug);
    /// # Ok(())
    /// # }
    /// ```
    pub fn global_id(&mut self, name: impl GlobalName) -> Result<GlobalId, Error> {
        name.to_global_id(&mut self.env)
    }

    /// Sets a global variable that'll be available to scripts executed by the engine.
    ///
    /// The `id` parameter can be either an `&str` or a prefetched [`global_id`][`Self::global_id`].
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, Value};
    ///
    /// let mut engine = Engine::new();
    /// engine.set("x", 1.0_f64);
    /// let _: Value = engine
    ///     .start("assertion.mi", "assert(x == 1)")?
    ///     .trampoline()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn set(&mut self, id: impl GlobalName, value: impl IntoValue) -> Result<(), Error> {
        let id = id.to_global_id(&mut self.env)?;
        self.globals.set(
            id.0,
            value.into_value_with_engine_state(&self.library, &mut self.gc).to_raw(&mut self.gc),
        );
        Ok(())
    }

    /// Returns the value of a global variable, or `nil` if it's not set.
    ///
    /// The `id` parameter can be either an `&str` or a prefetched [`global_id`][`Self::global_id`].
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, Value};
    ///
    /// let mut engine = Engine::new();
    /// let _: Value = engine
    ///     .start("assertion.mi", "let x = 1")?
    ///     .trampoline()?;
    /// let x: f64 = engine.get("x")?;
    /// assert_eq!(x, 1.0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn get<T>(&self, id: impl OptionalGlobalName) -> Result<T, Error>
    where
        T: TryFromValue,
    {
        if let Some(id) = id.try_to_global_id(&self.env) {
            T::try_from_value(&Value::from_raw(self.globals.get(id.0)), &self.library)
        } else {
            T::try_from_value(&Value::from_raw(RawValue::from(())), &self.library)
        }
    }

    /// Declares a "raw" function in the global scope. Raw functions do not perform any type checks
    /// by default and accept a variable number of arguments.
    ///
    /// You should generally prefer [`add_function`][`Self::add_function`] instead of this.
    ///
    /// Note that this cannot accept [`GlobalId`]s, because a name is required to create the
    /// function and global IDs have their name erased.
    ///
    /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
    /// function accepts a variable number of arguments. Note that because this function omits type
    /// checks you may receive a different amount of arguments than specified.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::{Engine, Value};
    /// use mica::ll::{bytecode::FunctionKind, value::RawValue};
    ///
    /// let mut engine = Engine::new();
    /// engine.add_raw_function(
    ///     "a_raw_understanding",
    ///     0,
    ///     FunctionKind::Foreign(Box::new(|env, gc, arguments| {
    ///         Ok(RawValue::from(1.0))
    ///     })),
    /// );
    /// let _: Value = engine
    ///     .start("assertion.mi", "assert(a_raw_understanding() == 1.0)")?
    ///     .trampoline()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_raw_function(
        &mut self,
        name: &str,
        parameter_count: impl Into<FunctionParameterCount>,
        f: FunctionKind,
    ) -> Result<(), Error> {
        let global_id = name.to_global_id(&mut self.env)?;
        let name = Rc::from(name);
        let function_id = self
            .env
            .create_function(Function {
                name: Rc::clone(&name),
                parameter_count: parameter_count.into(),
                kind: f,
                hidden_in_stack_traces: false,
            })
            .map_err(|_| Error::TooManyFunctions)?;
        let function =
            RawValue::from(self.gc.allocate(Closure { name, function_id, captures: Vec::new() }));
        self.globals.set(global_id.0, function);
        Ok(())
    }

    /// Declares a function in the global scope.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// use mica::Engine;
    ///
    /// let mut engine = Engine::new();
    /// engine.add_function("add", |x: f64, y: f64| x + y);
    /// let x: f64 = engine
    ///     .start("example.mi", "add(1, 2)")?
    ///     .trampoline()?;
    /// assert_eq!(x, 3.0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn add_function<F, V>(&mut self, name: &str, f: F) -> Result<(), Error>
    where
        V: ffvariants::BareMaybeVarargs,
        F: ForeignFunction<V, ParameterCount = FunctionParameterCount>,
    {
        self.add_raw_function(
            name,
            F::PARAMETER_COUNT,
            FunctionKind::Foreign(f.into_raw_foreign_function()),
        )
    }

    /// Declares a type in the global scope.
    ///
    /// # Examples
    /// See [`TypeBuilder<T>`] for examples.
    pub fn add_type<T>(&mut self, builder: TypeBuilder<T>) -> Result<(), Error>
    where
        T: UserData,
    {
        let built = builder.build_in_library(&mut self.env, &mut self.library, &mut self.gc)?;
        self.set_built_type(&built)?;
        Ok(())
    }

    pub(crate) fn set_built_type<T>(&mut self, typ: &BuiltType<T>) -> Result<(), Error>
    where
        T: Any,
    {
        let value = typ.make_type(&mut self.gc);
        self.set(typ.type_name.deref(), value)
    }

    /// Starts building a new trait.
    ///
    /// # Examples
    /// See [`TraitBuilder`] for examples.
    pub fn build_trait(&mut self, name: &str) -> Result<TraitBuilder<'_>, Error> {
        Ok(TraitBuilder {
            inner: codegen::TraitBuilder::new(&mut self.env, None, Rc::from(name)).mica()?,
            gc: &mut self.gc,
        })
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

/// A script pre-compiled into bytecode.
pub struct Script<'e> {
    engine: &'e mut Engine,
    main_chunk: Rc<Chunk>,
}

impl<'e> Script<'e> {
    /// Starts running a script in a new fiber.
    pub fn start(&mut self) -> Fiber {
        Fiber {
            engine: self.engine,
            inner: vm::Fiber::new(Rc::clone(&self.main_chunk), Vec::new()),
        }
    }

    /// Starts running a script in a new fiber, consuming the script.
    pub fn into_fiber(self) -> Fiber<'e> {
        Fiber {
            engine: self.engine,
            inner: vm::Fiber::new(Rc::clone(&self.main_chunk), Vec::new()),
        }
    }
}

impl<'e> fmt::Debug for Script<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Script").field("main_chunk", &self.main_chunk).finish_non_exhaustive()
    }
}

/// An ID unique to an engine, identifying a global variable.
///
/// Note that these IDs are not portable across different engine instances.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GlobalId(GlobalIndex);

mod global_id {
    use crate::GlobalId;

    pub trait Sealed {}
    impl Sealed for GlobalId {}
    impl Sealed for &str {}
}

/// A trait for names convertible to global IDs.
pub trait GlobalName: global_id::Sealed {
    #[doc(hidden)]
    fn to_global_id(&self, env: &mut Environment) -> Result<GlobalId, Error>;
}

impl GlobalName for GlobalId {
    fn to_global_id(&self, _: &mut Environment) -> Result<GlobalId, Error> {
        Ok(*self)
    }
}

impl GlobalName for &str {
    fn to_global_id(&self, env: &mut Environment) -> Result<GlobalId, Error> {
        Ok(if let Some(slot) = env.get_global(self) {
            GlobalId(slot)
        } else {
            env.create_global(self).map(GlobalId).map_err(|_| Error::TooManyGlobals)?
        })
    }
}

/// A trait for names convertible to global IDs.
pub trait OptionalGlobalName {
    #[doc(hidden)]
    fn try_to_global_id(&self, env: &Environment) -> Option<GlobalId>;
}

impl OptionalGlobalName for GlobalId {
    fn try_to_global_id(&self, _: &Environment) -> Option<GlobalId> {
        Some(*self)
    }
}

impl OptionalGlobalName for &str {
    fn try_to_global_id(&self, env: &Environment) -> Option<GlobalId> {
        env.get_global(self).map(GlobalId)
    }
}

mod method_id {
    use crate::MethodId;

    pub trait Sealed {}

    impl Sealed for MethodId {}
    impl Sealed for (&str, u8) {}
}

/// An ID unique to an engine, identifying a method signature.
///
/// Note that these IDs are not portable across different engine instances.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct MethodId(pub(crate) MethodIndex);

/// Implemented by every type that can be used as a method signature.
///
/// See [`Engine::call_method`].
pub trait MethodSignature: method_id::Sealed {
    #[doc(hidden)]
    fn to_method_id(&self, env: &mut Environment) -> Result<MethodId, Error>;
}

impl MethodSignature for MethodId {
    fn to_method_id(&self, _: &mut Environment) -> Result<MethodId, Error> {
        Ok(*self)
    }
}

/// Tuples of string slices and `u8`s are a user-friendly representation of method signatures.
/// For instance, `("cat", 1)` represents the method `cat/1`.
impl MethodSignature for (&str, u8) {
    fn to_method_id(&self, env: &mut Environment) -> Result<MethodId, Error> {
        env.get_or_create_method_index(&bytecode::MethodSignature {
            name: Rc::from(self.0),
            parameter_count: MethodParameterCount::from_count_without_self(self.1)
                .map_err(|_| Error::TooManyArguments)?,
            trait_id: None,
        })
        .map(MethodId)
        .map_err(|_| Error::TooManyMethods)
    }
}
