//! The virtual machine.

use crate::bytecode::{Chunk, Opcode, Opr24};
use crate::common::{Error, ErrorKind};
use crate::value::Value;

/// Storage for global variables.
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

/// The virtual machine state.
pub struct Vm {
   stack: Vec<Value>,
   breakable_block_stack: Vec<usize>,
}

impl Vm {
   /// Creates a new VM.
   pub fn new() -> Self {
      Self {
         stack: Vec::new(),
         breakable_block_stack: Vec::new(),
      }
   }

   fn error(chunk: &Chunk, pc: usize, kind: ErrorKind) -> Error {
      Error {
         kind,
         location: chunk.location(pc),
      }
   }

   fn wrap_error<T>(chunk: &Chunk, pc: usize, result: Result<T, ErrorKind>) -> Result<T, Error> {
      result.map_err(|kind| Self::error(chunk, pc, kind))
   }

   #[inline(always)]
   fn push(&mut self, value: Value) {
      self.stack.push(value);
   }

   #[inline(always)]
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

   #[inline(always)]
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

   #[inline(always)]
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

   /// Interprets bytecode in the chunk, with the provided global storage.
   pub fn interpret(&mut self, chunk: &Chunk, globals: &mut Globals) -> Result<Value, Error> {
      let mut pc = 0;

      self.stack.extend(
         std::iter::repeat_with(|| Value::Nil).take(chunk.preallocate_stack_slots as usize),
      );

      loop {
         let instruction_pc = pc;
         let opcode = unsafe { chunk.read_opcode(&mut pc) };

         macro_rules! wrap_error {
            ($exp:expr) => {{
               Self::wrap_error(chunk, instruction_pc, $exp)
            }};
         }

         macro_rules! binary_operator {
            ($op:tt) => {{
               let right = wrap_error!(self.pop().number())?;
               let left = wrap_error!(self.stack_top().number())?;
               *self.stack_top_mut() = Value::Number(left $op right);
            }};
         }

         match opcode {
            Opcode::Nop => (),

            Opcode::PushNil => self.push(Value::Nil),
            Opcode::PushTrue => self.push(Value::True),
            Opcode::PushFalse => self.push(Value::False),
            Opcode::PushNumber => {
               let number = unsafe { chunk.read_number(&mut pc) };
               self.push(Value::Number(number));
            }
            Opcode::PushString => {
               let string = unsafe { chunk.read_string(&mut pc) };
               self.push(Value::String(string.into()));
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
               self.stack[slot] = value;
            }
            Opcode::GetLocal(slot) => {
               let slot = u32::from(slot) as usize;
               let value = self.stack[slot].clone();
               self.push(value);
            }
            Opcode::Swap => {
               let len = self.stack.len();
               self.stack.swap(len - 2, len - 1);
            }
            Opcode::Discard => {
               self.stack.pop();
            }

            Opcode::JumpForward(amount) => {
               let amount = u32::from(amount) as usize;
               pc += amount;
            }
            Opcode::JumpForwardIfFalsy(amount) => {
               let amount = u32::from(amount) as usize;
               if self.stack_top().is_falsy() {
                  pc += amount;
               }
            }
            Opcode::JumpForwardIfTruthy(amount) => {
               let amount = u32::from(amount) as usize;
               if self.stack_top().is_truthy() {
                  pc += amount;
               }
            }
            Opcode::JumpBackward(amount) => {
               let amount = u32::from(amount) as usize;
               pc -= amount;
            }

            Opcode::EnterBreakableBlock => {
               self.breakable_block_stack.push(self.stack.len());
            }
            Opcode::ExitBreakableBlock(n) => {
               let result = self.pop();
               for _ in 0..n {
                  unsafe {
                     let top = self.breakable_block_stack.pop().unwrap_unchecked();
                     self.stack.set_len(top)
                  }
               }
               self.push(result);
            }

            Opcode::Negate => {
               // TODO: Debug info.
               let number = wrap_error!(self.pop().number())?;
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
               let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right))? {
                  ordering.is_lt()
               } else {
                  false
               };
               self.push(Value::from(is_less));
            }
            Opcode::LessEqual => {
               let right = self.pop();
               let left = self.pop();
               let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right))? {
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
         self.stack.len() - chunk.preallocate_stack_slots as usize,
         Value::Nil,
      );

      Ok(result)
   }
}
