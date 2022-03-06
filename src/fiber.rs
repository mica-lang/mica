use std::cell::RefCell;
use std::rc::Rc;

use mica_language::value::Value;
use mica_language::vm;

use crate::{Error, RuntimeEnvironment};

/// A fiber represents an independent, pausable thread of code execution.
///
/// # Reentrancy
///
/// Note that fibers are not reentrant: an engine can only be running a single fiber at a time,
/// due to Rust's exclusive mutability constraints. Trying to run two fibers at the same time will
/// result in a panic.
pub struct Fiber {
   pub(crate) runtime_env: Rc<RefCell<RuntimeEnvironment>>,
   pub(crate) inner: vm::Fiber,
}

impl Fiber {
   /// Resumes execution of a fiber. If execution is done already, returns `None`.
   pub fn resume(&mut self) -> Result<Option<Value>, Error> {
      if self.inner.halted() {
         Ok(None)
      } else {
         let mut runtime_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
         let (env, globals) = runtime_env.split();
         let result = self.inner.interpret(env, globals)?;
         Ok(Some(result))
      }
   }
}
