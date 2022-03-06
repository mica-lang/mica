use std::cell::RefCell;
use std::rc::Rc;

use mica_language::bytecode::{Chunk, Environment};
use mica_language::codegen::CodeGenerator;
use mica_language::lexer::Lexer;
use mica_language::parser::Parser;
use mica_language::vm::{self, Globals};

use crate::{Error, Fiber};

/// An execution engine. Contains information about things like globals, registered types, etc.
pub struct Engine {
   runtime_env: Rc<RefCell<RuntimeEnvironment>>,
}

impl Engine {
   /// Creates a new engine.
   pub fn new() -> Self {
      Self {
         runtime_env: Rc::new(RefCell::new(RuntimeEnvironment {
            env: Environment::new(),
            globals: Globals::new(),
         })),
      }
   }

   /// Compiles a script.
   pub fn compile(
      &self,
      filename: impl AsRef<str>,
      source: impl Into<String>,
   ) -> Result<Script, Error> {
      let module_name = Rc::from(filename.as_ref());
      let lexer = Lexer::new(Rc::clone(&module_name), source.into());
      let (ast, root_node) = Parser::new(lexer).parse()?;
      let mut shared_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
      let main_chunk =
         CodeGenerator::new(module_name, &mut shared_env.env).generate(&ast, root_node)?;

      Ok(Script {
         runtime_env: Rc::clone(&self.runtime_env),
         main_chunk,
      })
   }

   /// Compiles and starts running a script.
   ///
   /// This can be used as a shorthand if you don't intend to reuse the compiled bytecode.
   pub fn start(
      &self,
      filename: impl AsRef<str>,
      source: impl Into<String>,
   ) -> Result<Fiber, Error> {
      Ok(self.compile(filename, source)?.start())
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
