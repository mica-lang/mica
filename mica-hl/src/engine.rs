use std::any::Any;
use std::ops::Deref;
use std::rc::Rc;

use mica_language::ast::DumpAst;
use mica_language::bytecode::{
   BuiltinDispatchTables, Chunk, DispatchTable, Environment, Function, FunctionKind, Opr24,
};
use mica_language::codegen::CodeGenerator;
use mica_language::gc::{Gc, Memory};
use mica_language::lexer::Lexer;
use mica_language::parser::Parser;
use mica_language::value::{Closure, RawValue};
use mica_language::vm::{self, Globals};

use crate::{
   ffvariants, BuiltType, Error, Fiber, ForeignFunction, StandardLibrary, TryFromValue,
   TypeBuilder, UserData, Value,
};

pub use mica_language::bytecode::ForeignFunction as RawForeignFunction;

/// Options for debugging the language implementation.
#[derive(Debug, Clone, Copy, Default)]
pub struct DebugOptions {
   /// Set to `true` to print the AST to stdout after parsing.
   pub dump_ast: bool,
   /// Set to `true` to print the bytecode to stdout after successful compilation.
   pub dump_bytecode: bool,
}

/// An execution engine. Contains information about things like globals, registered types, etc.
pub struct Engine {
   pub(crate) env: Environment,
   pub(crate) globals: Globals,
   // This field is needed to keep all builtin dispatch tables alive for longer than `gc`.
   _builtin_refs: BuiltinRefs,
   pub(crate) gc: Memory,
   debug_options: DebugOptions,
}

struct BuiltinRefs {
   nil: Gc<DispatchTable>,
   boolean: Gc<DispatchTable>,
   number: Gc<DispatchTable>,
   string: Gc<DispatchTable>,
   function: Gc<DispatchTable>,
}

impl Engine {
   /// Creates a new engine.
   pub fn new(stdlib: impl StandardLibrary) -> Self {
      Self::with_debug_options(stdlib, Default::default())
   }

   /// Creates a new engine with specific debug options.
   ///
   /// [`Engine::new`] creates an engine with [`Default`] debug options, and should generally be
   /// preferred unless you're debugging the language's internals.
   ///
   /// Constructing the engine can fail if the standard library defines way too many methods.
   pub fn with_debug_options(
      mut stdlib: impl StandardLibrary,
      debug_options: DebugOptions,
   ) -> Self {
      let mut gc = Memory::new();
      // This is a little bad because it allocates a bunch of empty dtables only to discard them.
      let mut env = Environment::new(BuiltinDispatchTables::empty(&mut gc));

      macro_rules! get_dtables {
         ($type_name:tt, $define:tt) => {{
            let tb = TypeBuilder::new($type_name);
            let tb = stdlib.$define(tb);
            // Unwrapping here is fine because the stdlib should never declare THAT many methods.
            tb.build(&mut env, &mut gc).unwrap()
         }};
      }
      // TODO: Expose _*_type in the global scope.
      let nil = get_dtables!("Nil", define_nil);
      let boolean = get_dtables!("Boolean", define_boolean);
      let number = get_dtables!("Number", define_number);
      let string = get_dtables!("String", define_string);
      let refs = BuiltinRefs {
         nil: Gc::clone(&nil.instance_dtable),
         boolean: Gc::clone(&boolean.instance_dtable),
         number: Gc::clone(&number.instance_dtable),
         string: Gc::clone(&string.instance_dtable),
         function: unsafe {
            Gc::from_raw(gc.allocate(DispatchTable::new_for_instance("Function")))
         }, // TODO
      };
      env.builtin_dtables = BuiltinDispatchTables {
         nil: Gc::as_raw(&refs.nil),
         boolean: Gc::as_raw(&refs.boolean),
         number: Gc::as_raw(&refs.number),
         string: Gc::as_raw(&refs.string),
         function: Gc::as_raw(&refs.function),
      };

      let mut engine = Self {
         env,
         globals: Globals::new(),
         _builtin_refs: refs,
         gc,
         debug_options,
      };
      // Unwrapping here is fine because at this point we haven't got quite that many globals
      // registered to overflow an Opr24.
      engine.set_built_type(&nil).unwrap();
      engine.set_built_type(&boolean).unwrap();
      engine.set_built_type(&number).unwrap();
      engine.set_built_type(&string).unwrap();

      engine
   }

   /// Compiles a script.
   ///
   /// # Errors
   ///  - [`Error::Compile`] - Syntax or semantic error
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

      let main_chunk = CodeGenerator::new(module_name, &mut self.env).generate(&ast, root_node)?;
      if self.debug_options.dump_bytecode {
         eprintln!("Mica - global environment:");
         eprintln!("{:#?}", self.env);
         eprintln!("Mica - main chunk disassembly:");
         eprintln!("{:#?}", main_chunk);
      }

      Ok(Script {
         engine: self,
         main_chunk,
      })
   }

   /// Compiles and starts running a script.
   ///
   /// This can be used as a shorthand if you don't intend to reuse the compiled bytecode.
   ///
   /// # Errors
   /// See [`compile`][`Self::compile`].
   pub fn start(
      &mut self,
      filename: impl AsRef<str>,
      source: impl Into<String>,
   ) -> Result<Fiber, Error> {
      let script = self.compile(filename, source)?;
      Ok(script.into_fiber())
   }

   /// Returns the unique global ID for the global with the given name, or an error if there
   /// are too many globals in scope.
   ///
   /// The maximum amount of globals is about 16 million, so you shouldn't worry too much about
   /// hitting that limit unless you're stress-testing the VM or accepting untrusted input as
   /// globals.
   ///
   /// # Errors
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   pub fn global_id(&mut self, name: &str) -> Result<GlobalId, Error> {
      if let Some(slot) = self.env.get_global(name) {
         Ok(GlobalId(slot))
      } else {
         Ok(GlobalId(
            self.env.create_global(name).map_err(|_| Error::TooManyGlobals)?,
         ))
      }
   }

   /// Sets a global variable that'll be available to scripts executed by the engine.
   ///
   /// The `id` parameter can be either an `&str` or a prefetched [`global_id`][`Self::global_id`].
   ///
   /// # Errors
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   pub fn set<G, T>(&mut self, id: G, value: T) -> Result<(), Error>
   where
      G: ToGlobalId,
      T: Into<Value>,
   {
      let id = id.to_global_id(&mut self.env)?;
      self.globals.set(id.0, value.into().to_raw(&mut self.gc));
      Ok(())
   }

   /// Returns the value of a global variable, or `nil` if it's not set.
   ///
   /// The `id` parameter can be either an `&str` or a prefetched [`global_id`][`Self::global_id`].
   ///
   /// # Errors
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   ///  - [`Error::TypeMismatch`] - The type of the value is not convertible to `T`
   pub fn get<G, T>(&self, id: G) -> Result<T, Error>
   where
      G: TryToGlobalId,
      T: TryFromValue,
   {
      if let Some(id) = id.try_to_global_id(&self.env) {
         T::try_from_value(&Value::from_raw(self.globals.get(id.0)))
      } else {
         T::try_from_value(&Value::from_raw(RawValue::from(())))
      }
   }

   /// Declares a "raw" function in the global scope. Raw functions do not perform any type checks
   /// by default and accept a variable number of arguments.
   ///
   /// You should generally prefer [`add_function`][`Self::add_function`] instead of this.
   ///
   /// Note that this cannot accept [`GlobalId`]s, because a name is required to create the function
   /// and global IDs have their name erased.
   ///
   /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
   /// function accepts a variable number of arguments. Note that because this function omits type
   /// checks you may receive a different amount of arguments than specified.
   ///
   /// # Errors
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   ///  - [`Error::TooManyFunctions`] - Too many functions were registered into the engine
   pub fn add_raw_function(
      &mut self,
      name: &str,
      parameter_count: impl Into<Option<u16>>,
      f: RawForeignFunction,
   ) -> Result<(), Error> {
      let global_id = name.to_global_id(&mut self.env)?;
      let function_id = self
         .env
         .create_function(Function {
            name: Rc::from(name),
            parameter_count: parameter_count.into(), // doesn't matter for non-methods
            kind: FunctionKind::Foreign(f),
         })
         .map_err(|_| Error::TooManyFunctions)?;
      let function = RawValue::from(self.gc.allocate(Closure {
         function_id,
         captures: Vec::new(),
      }));
      self.globals.set(global_id.0, function);
      Ok(())
   }

   /// Declares a function in the global scope.
   ///
   /// # Errors
   /// See [`add_raw_function`][`Self::add_raw_function`].
   pub fn add_function<F, V>(&mut self, name: &str, f: F) -> Result<(), Error>
   where
      V: ffvariants::Bare,
      F: ForeignFunction<V>,
   {
      self.add_raw_function(name, F::parameter_count(), f.into_raw_foreign_function())
   }

   /// Declares a type in the global scope.
   ///
   /// # Errors
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   ///  - [`Error::TooManyFunctions`] - Too many functions were registered into the engine
   ///  - [`Error::TooManyMethods`] - Too many unique method signatures were created
   pub fn add_type<T>(&mut self, builder: TypeBuilder<T>) -> Result<(), Error>
   where
      T: Any + UserData,
   {
      let built = builder.build(&mut self.env, &mut self.gc)?;
      self.set_built_type(&built)?;
      Ok(())
   }

   pub(crate) fn set_built_type<T>(&mut self, typ: &BuiltType<T>) -> Result<(), Error>
   where
      T: Any,
   {
      let value = typ.make_type();
      self.set(typ.type_name.deref(), value)
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
         inner: vm::Fiber::new(Rc::clone(&self.main_chunk)),
      }
   }

   /// Starts running a script in a new fiber, consuming the script.
   pub fn into_fiber(self) -> Fiber<'e> {
      Fiber {
         engine: self.engine,
         inner: vm::Fiber::new(Rc::clone(&self.main_chunk)),
      }
   }
}

/// An ID unique to an engine, identifying a global variable.
///
/// Note that these IDs are not portable across different engine instances.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GlobalId(Opr24);

mod global_id {
   use crate::GlobalId;

   pub trait Sealed {}
   impl Sealed for GlobalId {}
   impl Sealed for &str {}
}

/// A trait for names convertible to global IDs.
pub trait ToGlobalId: global_id::Sealed {
   #[doc(hidden)]
   fn to_global_id(&self, env: &mut Environment) -> Result<GlobalId, Error>;
}

impl ToGlobalId for GlobalId {
   fn to_global_id(&self, _: &mut Environment) -> Result<GlobalId, Error> {
      Ok(*self)
   }
}

impl ToGlobalId for &str {
   fn to_global_id(&self, env: &mut Environment) -> Result<GlobalId, Error> {
      Ok(if let Some(slot) = env.get_global(*self) {
         GlobalId(slot)
      } else {
         env.create_global(*self).map(GlobalId).map_err(|_| Error::TooManyGlobals)?
      })
   }
}

/// A trait for names convertible to global IDs.
pub trait TryToGlobalId {
   #[doc(hidden)]
   fn try_to_global_id(&self, env: &Environment) -> Option<GlobalId>;
}

impl TryToGlobalId for GlobalId {
   fn try_to_global_id(&self, _: &Environment) -> Option<GlobalId> {
      Some(*self)
   }
}

impl TryToGlobalId for &str {
   fn try_to_global_id(&self, env: &Environment) -> Option<GlobalId> {
      env.get_global(*self).map(GlobalId)
   }
}
