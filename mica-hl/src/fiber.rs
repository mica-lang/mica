use std::cell::RefCell;
use std::rc::Rc;

use mica_language::value::Value;
use mica_language::vm;

use crate::{Error, RuntimeEnvironment, TryFromValue};

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
   pub fn resume<T>(&mut self) -> Result<Option<T>, Error>
   where
      T: TryFromValue,
   {
      if self.inner.halted() {
         Ok(None)
      } else {
         let mut runtime_env = self.runtime_env.try_borrow_mut().map_err(|_| Error::EngineInUse)?;
         let (env, globals) = runtime_env.split();
         let result = self.inner.interpret(env, globals)?;
         Ok(Some(T::try_from_value(result)?))
      }
   }

   /// Resumes execution of a fiber until it's done evaluating all code. The last result is
   /// returned and results from intermediate yields are discarded.
   ///
   /// This consumes the fiber, as calling one that's finished is not useful.
   pub fn trampoline<T>(mut self) -> Result<T, Error>
   where
      T: TryFromValue,
   {
      let mut result = Value::Nil;
      while let Some(v) = self.resume()? {
         result = v;
      }
      T::try_from_value(result)
   }
}
