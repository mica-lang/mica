use std::fmt;

use crate::{ll::vm, Engine, Error, TryFromValue, Value};

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
            let Engine { env, globals, gc, .. } = &mut self.engine;
            let result = self.inner.interpret(env, globals, gc)?;
            Ok(Some(T::try_from_value(&Value::from_raw(result), &self.engine.env)?))
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
        T::try_from_value(&result, &self.engine.env)
    }
}

impl<'e> fmt::Debug for Fiber<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Fiber").finish_non_exhaustive()
    }
}
