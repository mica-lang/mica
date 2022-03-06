//! The virtual machine.

use std::rc::Rc;

use crate::bytecode::{Chunk, Environment, FunctionKind, Opcode, Opr24};
use crate::common::{Error, ErrorKind, StackTraceEntry};
use crate::value::{Closure, Value};

/// Storage for global variables.
#[derive(Debug)]
pub struct Globals {
   values: Vec<Value>,
}

impl Globals {
   pub fn new() -> Self {
      Self { values: Vec::new() }
   }

   pub fn set(&mut self, slot: Opr24, value: Value) {
      let slot = u32::from(slot) as usize;
      if slot >= self.values.len() {
         self.values.resize(slot + 1, Value::Nil);
      }
      // Safety: the vec has just been extended to have the correct size so `slot` is in bounds.
      unsafe {
         *self.values.get_unchecked_mut(slot) = value;
      }
   }

   pub fn get(&self, slot: Opr24) -> Value {
      let slot = u32::from(slot) as usize;
      self.values.get(slot).cloned().unwrap_or(Value::Nil)
   }

   unsafe fn get_unchecked(&self, slot: Opr24) -> Value {
      let slot = u32::from(slot) as usize;
      self.values.get_unchecked(slot).clone()
   }
}

impl Default for Globals {
   fn default() -> Self {
      Self::new()
   }
}

struct ReturnPoint {
   chunk: Rc<Chunk>,
   pc: usize,
   stack_bottom: usize,
}

/// The virtual machine state.
pub struct Fiber {
   pc: usize,
   chunk: Rc<Chunk>,

   stack: Vec<Value>,
   stack_bottom: usize,
   call_stack: Vec<ReturnPoint>,
   breakable_block_stack: Vec<usize>,
}

impl Fiber {
   /// Creates a new VM.
   pub fn new(chunk: Rc<Chunk>) -> Self {
      Self {
         pc: 0,
         chunk,
         stack: Vec::new(),
         stack_bottom: 0,
         call_stack: Vec::new(),
         breakable_block_stack: Vec::new(),
      }
   }

   fn error(&self, kind: ErrorKind) -> Error {
      Error::Runtime {
         kind,
         call_stack: self
            .call_stack
            .iter()
            .map(|return_point| StackTraceEntry {
               function_name: Rc::from("<function names TODO>"),
               module_name: Rc::clone(&return_point.chunk.module_name),
               location: return_point.chunk.location(return_point.pc - Opcode::SIZE),
            })
            .collect(),
      }
   }

   fn push(&mut self, value: Value) {
      self.stack.push(value);
   }

   fn pop(&mut self) -> Value {
      #[cfg(debug_assertions)]
      {
         self.stack.pop().unwrap()
      }
      #[cfg(not(debug_assertions))]
      unsafe {
         self.stack.pop().unwrap_unchecked()
      }
   }

   fn stack_top(&self) -> &Value {
      #[cfg(debug_assertions)]
      {
         self.stack.last().unwrap()
      }
      #[cfg(not(debug_assertions))]
      unsafe {
         self.stack.get_unchecked(self.stack.len() - 1)
      }
   }

   fn stack_top_mut(&mut self) -> &mut Value {
      #[cfg(debug_assertions)]
      {
         self.stack.last_mut().unwrap()
      }
      #[cfg(not(debug_assertions))]
      unsafe {
         let last = self.stack.len() - 1;
         self.stack.get_unchecked_mut(last)
      }
   }

   fn nth_from_top(&self, n: usize) -> &Value {
      #[cfg(debug_assertions)]
      {
         &self.stack[self.stack.len() - n]
      }
      #[cfg(not(debug_assertions))]
      unsafe {
         let i = self.stack.len() - n;
         self.stack.get_unchecked(i)
      }
   }

   fn save_return_point(&mut self) {
      self.call_stack.push(ReturnPoint {
         chunk: Rc::clone(&self.chunk),
         pc: self.pc,
         stack_bottom: self.stack_bottom,
      });
   }

   fn restore_return_point(&mut self) {
      let return_point = self.call_stack.pop().unwrap();
      // Remove unnecessary values from the stack (eg. arguments).
      while self.stack.len() > self.stack_bottom {
         self.stack.pop().unwrap();
      }
      // Restore state.
      self.chunk = return_point.chunk;
      self.pc = return_point.pc;
      self.stack_bottom = return_point.stack_bottom;
   }

   fn allocate_chunk_storage_slots(&mut self, n: usize) {
      self.stack.extend(std::iter::repeat_with(|| Value::Nil).take(n));
   }

   /// Interprets bytecode in the chunk, with the provided global storage.
   pub fn interpret(
      &mut self,
      env: &mut Environment,
      globals: &mut Globals,
   ) -> Result<Value, Error> {
      self.allocate_chunk_storage_slots(self.chunk.preallocate_stack_slots as usize);

      loop {
         let opcode = unsafe { self.chunk.read_opcode(&mut self.pc) };

         macro_rules! wrap_error {
            ($exp:expr) => {{
               let result = $exp;
               match result {
                  Ok(value) => value,
                  Err(kind) => {
                     self.save_return_point();
                     return Err(self.error(kind));
                  }
               }
            }};
         }

         macro_rules! binary_operator {
            ($op:tt) => {{
               let right = wrap_error!(self.pop().number());
               let left = wrap_error!(self.stack_top().number());
               *self.stack_top_mut() = Value::Number(left $op right);
            }};
         }

         match opcode {
            Opcode::Nop => (),

            Opcode::PushNil => self.push(Value::Nil),
            Opcode::PushTrue => self.push(Value::True),
            Opcode::PushFalse => self.push(Value::False),
            Opcode::PushNumber => {
               let number = unsafe { self.chunk.read_number(&mut self.pc) };
               self.push(Value::Number(number));
            }
            Opcode::PushString => {
               let string = Rc::from(unsafe { self.chunk.read_string(&mut self.pc) });
               self.push(Value::String(string));
            }
            Opcode::CreateClosure(function_id) => {
               self.push(Value::Function(Rc::new(Closure { function_id })));
            }

            Opcode::AssignGlobal(slot) => {
               let value = self.stack_top().clone();
               globals.set(slot, value);
            }
            Opcode::GetGlobal(slot) => {
               let value = unsafe { globals.get_unchecked(slot) };
               self.push(value);
            }
            Opcode::AssignLocal(slot) => {
               let slot = u32::from(slot) as usize;
               let value = self.stack_top().clone();
               self.stack[self.stack_bottom + slot] = value;
            }
            Opcode::GetLocal(slot) => {
               let slot = u32::from(slot) as usize;
               let value = self.stack[self.stack_bottom + slot].clone();
               self.push(value);
            }

            Opcode::Swap => {
               let len = self.stack.len();
               self.stack.swap(len - 2, len - 1);
            }
            Opcode::Discard => {
               self.pop();
            }

            Opcode::JumpForward(amount) => {
               let amount = u32::from(amount) as usize;
               self.pc += amount;
            }
            Opcode::JumpForwardIfFalsy(amount) => {
               let amount = u32::from(amount) as usize;
               if self.stack_top().is_falsy() {
                  self.pc += amount;
               }
            }
            Opcode::JumpForwardIfTruthy(amount) => {
               let amount = u32::from(amount) as usize;
               if self.stack_top().is_truthy() {
                  self.pc += amount;
               }
            }
            Opcode::JumpBackward(amount) => {
               let amount = u32::from(amount) as usize;
               self.pc -= amount;
            }

            Opcode::EnterBreakableBlock => {
               self.breakable_block_stack.push(self.stack.len());
            }
            Opcode::ExitBreakableBlock(n) => {
               let result = self.pop();
               for _ in 0..n {
                  unsafe {
                     let top = self.breakable_block_stack.pop().unwrap_unchecked();
                     while self.stack.len() > top {
                        self.stack.pop().unwrap();
                     }
                  }
               }
               self.push(result);
            }

            Opcode::Call(argument_count) => {
               let argument_count = argument_count as usize;
               let function_value = self.nth_from_top(argument_count + 1);
               let closure = wrap_error!(function_value.function());
               let function = unsafe { env.get_function_unchecked(closure.function_id) };
               match &function.kind {
                  FunctionKind::Bytecode(chunk) => {
                     self.save_return_point();
                     self.chunk = Rc::clone(chunk);
                     self.pc = 0;
                     self.stack_bottom = self.stack.len() - argument_count;
                     self.allocate_chunk_storage_slots(chunk.preallocate_stack_slots as usize);
                  }
                  FunctionKind::Foreign(f) => {
                     let arguments =
                        unsafe { self.stack.get_unchecked(self.stack.len() - argument_count..) };
                     let result = f(arguments);
                     self.push(result);
                  }
               }
            }
            Opcode::Return => {
               let result = self.pop();
               self.restore_return_point();
               // Remove the function from the stack.
               self.stack.pop().unwrap();
               // Push the result onto the stack.
               self.push(result);
            }

            Opcode::Negate => {
               // TODO: Debug info.
               let number = wrap_error!(self.pop().number());
               self.push(Value::Number(-number))
            }
            Opcode::Add => binary_operator!(+),
            Opcode::Subtract => binary_operator!(-),
            Opcode::Multiply => binary_operator!(*),
            Opcode::Divide => binary_operator!(/),

            Opcode::Not => {
               let value = self.pop();
               self.push(Value::from(!value.is_truthy()));
            }
            Opcode::Equal => {
               let right = self.pop();
               let left = self.pop();
               self.push(Value::from(left == right));
            }
            Opcode::Less => {
               let right = self.pop();
               let left = self.pop();
               let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right)) {
                  ordering.is_lt()
               } else {
                  false
               };
               self.push(Value::from(is_less));
            }
            Opcode::LessEqual => {
               let right = self.pop();
               let left = self.pop();
               let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right)) {
                  ordering.is_le()
               } else {
                  false
               };
               self.push(Value::from(is_less));
            }

            Opcode::Halt => break,

            Opcode::__Padding(_) => unreachable!(),
         }
      }

      let result = self.stack.pop().expect("no result found on the top of the stack");

      self.stack.resize(
         self.stack.len() - self.chunk.preallocate_stack_slots as usize,
         Value::Nil,
      );

      Ok(result)
   }
}
