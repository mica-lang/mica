use std::cell::RefCell;
use std::rc::Rc;

use mica_language::ast::DumpAst;
use mica_language::bytecode::{Chunk, Environment, Function, FunctionKind, Opr24};
use mica_language::codegen::CodeGenerator;
use mica_language::lexer::Lexer;
use mica_language::parser::Parser;
use mica_language::value::{Closure, Value};
use mica_language::vm::{self, Globals};

use crate::{Error, Fiber, ToValue, TryFromValue};

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
   runtime_env: Rc<RefCell<RuntimeEnvironment>>,
   debug_options: DebugOptions,
}

impl Engine {
   /// Creates a new engine.
   pub fn new() -> Self {
      Self::with_debug_options(Default::default())
   }

   /// Creates a new engine with specific debug options.
   ///
   /// [`Engine::new`] creates an engine with [`Default`] debug options, and should generally be
   /// preferred unless you're debugging the language's internals.
   pub fn with_debug_options(debug_options: DebugOptions) -> Self {
      Self {
         runtime_env: Rc::new(RefCell::new(RuntimeEnvironment {
            env: Environment::new(),
            globals: Globals::new(),
         })),
         debug_options,
      }
   }

   /// Compiles a script.
   ///
   /// # Errors
   ///  - [`Error::EngineInUse`] - A fiber is currently running in this engine
   ///  - [`Error::Compile`] - Syntax or semantic error
   pub fn compile(
      &self,
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

      let mut shared_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
      let main_chunk =
         CodeGenerator::new(module_name, &mut shared_env.env).generate(&ast, root_node)?;
      if self.debug_options.dump_bytecode {
         eprintln!("Mica - global environment:");
         eprintln!("{:#?}", shared_env.env);
         eprintln!("Mica - main chunk disassembly:");
         eprintln!("{:#?}", main_chunk);
      }

      Ok(Script {
         runtime_env: Rc::clone(&self.runtime_env),
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
      &self,
      filename: impl AsRef<str>,
      source: impl Into<String>,
   ) -> Result<Fiber, Error> {
      Ok(self.compile(filename, source)?.start())
   }

   /// Returns the unique global ID for the global with the given name, or an error if there
   /// are too many globals in scope.
   ///
   /// The maximum amount of globals is about 16 million, so you shouldn't worry too much about
   /// hitting that limit unless you're stress-testing the VM or accepting untrusted input as
   /// globals.
   ///
   /// # Errors
   ///  - [`Error::EngineInUse`] - A fiber is currently running in this engine
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   pub fn global_id(&self, name: &str) -> Result<GlobalId, Error> {
      let mut runtime_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
      if let Some(slot) = runtime_env.env.get_global(name) {
         Ok(GlobalId(slot))
      } else {
         Ok(GlobalId(
            runtime_env.env.create_global(name).map_err(|_| Error::TooManyGlobals)?,
         ))
      }
   }

   /// Sets a global variable that'll be available to scripts executed by the engine.
   ///
   /// The `id` parameter can be either an `&str` or a prefetched [`global_id`][`Self::global_id`].
   ///
   /// # Errors
   ///  - [`Error::EngineInUse`] - A fiber is currently running in this engine
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   pub fn set<G, T>(&self, id: G, value: T) -> Result<(), Error>
   where
      G: ToGlobalId,
      T: ToValue,
   {
      let mut runtime_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
      let id = id.to_global_id(&mut runtime_env.env)?;
      runtime_env.globals.set(id.0, value.to_value());
      Ok(())
   }

   /// Returns the value of a global variable, or `nil` if it's not set.
   ///
   /// The `id` parameter can be either an `&str` or a prefetched [`global_id`][`Self::global_id`].
   ///
   /// # Errors
   ///  - [`Error::EngineInUse`] - A fiber is currently running in this engine
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   ///  - [`Error::TypeMismatch`] - The type of the value is not convertible to `T`
   pub fn get<G, T>(&self, id: G) -> Result<T, Error>
   where
      G: ToGlobalId,
      T: TryFromValue,
   {
      let mut runtime_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
      let id = id.to_global_id(&mut runtime_env.env)?;
      T::try_from_value(runtime_env.globals.get(id.0))
   }

   /// Declares a "raw" function in the global scope. Raw functions do not perform any type checks
   /// by default and accept a variable number of arguments.
   ///
   /// Note that this cannot accept [`GlobalId`]s, because a name is required to create the function
   /// and global IDs have their name erased.
   ///
   /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
   /// function accepts a variable number of arguments. Note that because this function omits type
   /// checks you may receive a different amount of arguments than specified.
   ///
   /// # Errors
   ///  - [`Error::EngineInUse`] - A fiber is currently running in this engine
   ///  - [`Error::TooManyGlobals`] - Too many globals with unique names were created
   pub fn raw_function(
      &self,
      name: &str,
      parameter_count: impl Into<Option<u16>>,
      f: Box<dyn FnMut(&[Value]) -> Value>,
   ) -> Result<(), Error> {
      let mut runtime_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
      let global_id = name.to_global_id(&mut runtime_env.env)?;
      let function_id = runtime_env
         .env
         .create_function(Function {
            name: Rc::from(name),
            parameter_count: parameter_count.into(), // doesn't matter for non-methods
            kind: FunctionKind::Foreign(f),
         })
         .map_err(|_| Error::TooManyFunctions)?;
      let function = Value::Function(Rc::new(Closure { function_id }));
      runtime_env.globals.set(global_id.0, function);
      Ok(())
   }
}

impl Default for Engine {
   fn default() -> Self {
      Self::new()
   }
}

/// The runtime environment. Contains declared globals and their values.
pub(crate) struct RuntimeEnvironment {
   pub(crate) env: Environment,
   pub(crate) globals: Globals,
}

impl RuntimeEnvironment {
   pub fn split(&mut self) -> (&mut Environment, &mut Globals) {
      (&mut self.env, &mut self.globals)
   }
}

/// A script pre-compiled into bytecode.
pub struct Script {
   runtime_env: Rc<RefCell<RuntimeEnvironment>>,
   main_chunk: Rc<Chunk>,
}

impl Script {
   /// Starts running a script in a new fiber.
   pub fn start(&self) -> Fiber {
      Fiber {
         runtime_env: Rc::clone(&self.runtime_env),
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
