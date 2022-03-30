use mica_language::value::Value;
use mica_language::vm;

use crate::{Engine, Error, TryFromValue};

/// A fiber represents an independent, pausable thread of code execution.
pub struct Fiber<'e> {
   pub(crate) engine: &'e mut Engine,
   pub(crate) inner: vm::Fiber,
}

impl<'e> Fiber<'e> {
   /// Resumes execution of a fiber. If execution is done already, returns `None`.
   pub fn resume<T>(&mut self) -> Result<Option<T>, Error>
   where
      T: TryFromValue,
   {
      if self.inner.halted() {
         Ok(None)
      } else {
         let result = self.inner.interpret(&mut self.engine.env, &mut self.engine.globals)?;
         Ok(Some(T::try_from_value(&result)?))
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
      let mut result = Value::from(());
      while let Some(v) = self.resume()? {
         result = v;
      }
      T::try_from_value(&result)
   }
}
