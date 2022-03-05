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
      self.values[slot] = value;
   }

   pub fn get(&mut self, slot: Opr24) -> Value {
      let slot = u32::from(slot) as usize;
      self.values.get(slot).cloned().unwrap_or(Value::Nil)
   }
}

/// The virtual machine state.
pub struct Vm {
   stack: Vec<Value>,
}

impl Vm {
   /// Creates a new VM.
   pub fn new() -> Self {
      Self { stack: Vec::new() }
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

   /// Interprets bytecode in the chunk.
   pub fn interpret(&mut self, chunk: &Chunk, globals: &mut Globals) -> Result<Value, Error> {
      let mut pc = 0;

      self.stack.extend(
         std::iter::repeat_with(|| Value::Nil).take(chunk.preallocate_stack_slots as usize),
      );

      while !chunk.at_end(pc) {
         let instruction_pc = pc;
         let opcode = chunk.read_opcode(&mut pc);

         macro_rules! wrap_error {
            ($exp:expr) => {{
               Self::wrap_error(chunk, instruction_pc, $exp)
            }};
         }

         macro_rules! binary_operator {
            ($op:tt) => {{
               let right = wrap_error!(self.stack.pop().unwrap().number())?;
               let left = wrap_error!(self.stack.pop().unwrap().number())?;
               self.stack.push(Value::Number(left $op right))
            }};
         }

         match opcode {
            Opcode::Nop => (),

            Opcode::PushNil => self.stack.push(Value::Nil),
            Opcode::PushTrue => self.stack.push(Value::True),
            Opcode::PushFalse => self.stack.push(Value::False),
            Opcode::PushNumber => {
               let number = chunk.read_number(&mut pc);
               self.stack.push(Value::Number(number));
            }
            Opcode::PushString => {
               let string = chunk.read_string(&mut pc);
               self.stack.push(Value::String(string.into()));
            }
            Opcode::AssignGlobal(slot) => {
               let value = self.stack.last().unwrap().clone();
               globals.set(slot, value);
            }
            Opcode::GetGlobal(slot) => {
               let value = globals.get(slot);
               self.stack.push(value);
            }
            Opcode::AssignLocal(slot) => {
               let slot = u32::from(slot) as usize;
               let value = self.stack.last().unwrap().clone();
               self.stack[slot] = value;
            }
            Opcode::GetLocal(slot) => {
               let slot = u32::from(slot) as usize;
               let value = self.stack[slot].clone();
               self.stack.push(value);
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
               if self.stack.last().unwrap().is_falsy() {
                  pc += amount;
               }
            }
            Opcode::JumpBackward(amount) => {
               let amount = u32::from(amount) as usize;
               pc -= amount;
            }

            Opcode::Negate => {
               // TODO: Debug info.
               let number = wrap_error!(self.stack.pop().unwrap().number())?;
               self.stack.push(Value::Number(-number))
            }
            Opcode::Add => binary_operator!(+),
            Opcode::Subtract => binary_operator!(-),
            Opcode::Multiply => binary_operator!(*),
            Opcode::Divide => binary_operator!(/),

            Opcode::Not => {
               let value = self.stack.pop().unwrap();
               self.stack.push(Value::from(!value.is_truthy()));
            }
            Opcode::Equal => {
               let right = self.stack.pop().unwrap();
               let left = self.stack.pop().unwrap();
               self.stack.push(Value::from(left == right));
            }
            Opcode::Less => {
               let right = self.stack.pop().unwrap();
               let left = self.stack.pop().unwrap();
               let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right))? {
                  ordering.is_lt()
               } else {
                  false
               };
               self.stack.push(Value::from(is_less));
            }
            Opcode::LessEqual => {
               let right = self.stack.pop().unwrap();
               let left = self.stack.pop().unwrap();
               let is_less = if let Some(ordering) = wrap_error!(left.try_partial_cmp(&right))? {
                  ordering.is_le()
               } else {
                  false
               };
               self.stack.push(Value::from(is_less));
            }

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
