//! The virtual machine.

use std::pin::Pin;
use std::ptr;
use std::rc::Rc;

use crate::bytecode::{
   CaptureKind, Chunk, DispatchTable, Environment, FunctionKind, FunctionSignature, Opcode, Opr24,
};
use crate::common::{Error, ErrorKind, Location, StackTraceEntry};
use crate::value::{Closure, Struct, Upvalue, Value};

/// Storage for global variables.
#[derive(Debug)]
pub struct Globals {
   values: Vec<Value>,
}

impl Globals {
   /// Creates a new storage.
   pub fn new() -> Self {
      Self { values: Vec::new() }
   }

   /// Sets the global in the given slot.
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

   /// Returns the global in the given slot, or `Nil` if there's no global there.
   pub fn get(&self, slot: Opr24) -> Value {
      let slot = u32::from(slot) as usize;
      self.values.get(slot).cloned().unwrap_or(Value::Nil)
   }

   /// Returns the global in the given slot without performing bounds checks.
   ///
   /// # Safety
   ///
   /// This is only safe to use by the VM. The code generator ensures that all globals are set
   /// before use.
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

/// The return point saved before entering a functi
struct ReturnPoint {
   chunk: Option<Rc<Chunk>>,
   closure: Option<Rc<Closure>>,
   pc: usize,
   stack_bottom: usize,
}

/// The virtual machine state.
pub struct Fiber {
   pc: usize,
   chunk: Rc<Chunk>,
   closure: Option<Rc<Closure>>,

   stack: Vec<Value>,
   stack_bottom: usize,
   open_upvalues: Vec<(u32, Pin<Rc<Upvalue>>)>,
   call_stack: Vec<ReturnPoint>,
   breakable_block_stack: Vec<usize>,

   halted: bool,
}

impl Fiber {
   /// Creates a new VM.
   pub fn new(chunk: Rc<Chunk>) -> Self {
      Self {
         pc: 0,
         chunk,
         closure: None,
         stack: Vec::new(),
         stack_bottom: 0,
         open_upvalues: Vec::new(),
         call_stack: Vec::new(),
         breakable_block_stack: Vec::new(),
         halted: false,
      }
   }

   /// Returns whether the fiber has halted execution.
   pub fn halted(&self) -> bool {
      self.halted
   }

   /// Halts the VM and produces an error.
   fn error(&mut self, env: &Environment, kind: ErrorKind) -> Error {
      self.halted = true;
      Error::Runtime {
         kind,
         call_stack: self
            .call_stack
            .iter()
            .map(|return_point| StackTraceEntry {
               function_name: if let Some(closure) = &return_point.closure {
                  let function = unsafe { env.get_function_unchecked(closure.function_id) };
                  Rc::clone(&function.name)
               } else {
                  Rc::from("<main>")
               },
               module_name: if let Some(chunk) = &return_point.chunk {
                  Rc::clone(&chunk.module_name)
               } else {
                  Rc::from("<FFI>")
               },
               location: if let Some(chunk) = &return_point.chunk {
                  chunk.location(return_point.pc - Opcode::SIZE)
               } else {
                  Location::UNINIT
               },
            })
            .collect(),
      }
   }

   /// Pushes a value onto the stack.
   fn push(&mut self, value: Value) {
      self.stack.push(value);
   }

   /// Pops a value off the stack.
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

   /// Returns a reference to the value at the top of the stack.
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

   /// Returns a mutable reference to the value at the top of the stack.
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

   /// Returns a reference to the `n`th value counted from the top of the stack.
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

   /// Saves the current program counter and other execution state before entering a function.
   fn save_return_point(&mut self) {
      self.call_stack.push(ReturnPoint {
         chunk: Some(Rc::clone(&self.chunk)),
         closure: self.closure.clone(),
         pc: self.pc,
         stack_bottom: self.stack_bottom,
      });
   }

   /// Restores previous VM state from the return point stack.
   fn restore_return_point(&mut self) {
      let return_point = self.call_stack.pop().unwrap();
      // Remove unnecessary values from the stack (eg. arguments).
      while self.stack.len() > self.stack_bottom {
         self.stack.pop().unwrap();
      }
      // Restore state.
      // SAFETY: Assume the matching chunk is a bytecode chunk.
      self.chunk = unsafe { return_point.chunk.unwrap_unchecked() };
      self.closure = return_point.closure;
      self.pc = return_point.pc;
      self.stack_bottom = return_point.stack_bottom;
   }

   /// Returns an upvalue for the local at the given stack slot.
   fn get_upvalue_for_local(&mut self, stack_slot: u32) -> Pin<Rc<Upvalue>> {
      if let Some((_, upvalue)) =
         self.open_upvalues.iter().rev().find(|(slot, _)| *slot == stack_slot)
      {
         Pin::clone(upvalue)
      } else {
         let stack_ptr = &mut self.stack[stack_slot as usize] as *mut _;
         // SAFETY: Vec storage is never null.
         let stack_ptr = unsafe { ptr::NonNull::new_unchecked(stack_ptr) };
         let upvalue = Upvalue::new(stack_ptr);
         self.open_upvalues.push((stack_slot, Pin::clone(&upvalue)));
         upvalue
      }
   }

   /// Allocates `n` storage slots for local variables.
   fn allocate_chunk_storage_slots(&mut self, n: usize) {
      self.stack.extend(std::iter::repeat_with(|| Value::Nil).take(n));
   }

   /// Calls a function. For bytecode functions this saves the stack and begins executing the
   /// function's chunk. For foreign functions it simply calls the function.
   fn enter_function(
      &mut self,
      s: &mut impl UserState,
      closure: Rc<Closure>,
      argument_count: usize,
   ) -> Result<(), Error> {
      let function = unsafe { s.env_mut().get_function_unchecked_mut(closure.function_id) };
      match &mut function.kind {
         FunctionKind::Bytecode { chunk, .. } => {
            self.save_return_point();
            self.chunk = Rc::clone(chunk);
            self.closure = Some(closure);
            self.pc = 0;
            self.stack_bottom = self.stack.len() - argument_count;
            self.allocate_chunk_storage_slots(chunk.preallocate_stack_slots as usize);
         }
         FunctionKind::Foreign(f) => {
            let arguments =
               unsafe { self.stack.get_unchecked(self.stack.len() - argument_count..) };
            let result = match f(arguments) {
               Ok(value) => value,
               Err(kind) => {
                  return Err(self.error_outside_function_call(Some(closure), s.env_mut(), kind));
               }
            };
            self.push(result);
         }
      }
      Ok(())
   }

   /// Constructs an error that wasn't triggered by a function call.
   fn error_outside_function_call(
      &mut self,
      closure: Option<Rc<Closure>>,
      env: &mut Environment,
      kind: ErrorKind,
   ) -> Error {
      self.save_return_point();
      if let Some(closure) = closure.as_ref() {
         self.call_stack.push(ReturnPoint {
            chunk: None,
            closure: Some(Rc::clone(closure)),
            pc: 0,
            stack_bottom: 0,
         });
      }
      let error = self.error(env, kind);
      if closure.is_some() {
         self.call_stack.pop();
      }
      self.call_stack.pop();
      error
   }

   /// Constructs a closure from surrounding stack variables and upvalues.
   fn create_closure(&mut self, env: &mut Environment, function_id: Opr24) -> Rc<Closure> {
      let function = unsafe { env.get_function_unchecked_mut(function_id) };
      let mut captures = Vec::new();
      if let FunctionKind::Bytecode {
         captured_locals, ..
      } = &function.kind
      {
         for capture in captured_locals {
            captures.push(match capture {
               CaptureKind::Local(slot) => {
                  self.get_upvalue_for_local(self.stack_bottom as u32 + u32::from(*slot))
               }
               CaptureKind::Upvalue(index) => {
                  let closure = self.closure.as_ref().unwrap();
                  Pin::clone(&closure.captures[u32::from(*index) as usize])
               }
            });
         }
      }
      Rc::new(Closure {
         function_id,
         captures,
      })
   }

   /// Returns the dispatch table of the given value.
   fn get_dispatch_table<'v>(value: &'v Value, env: &'v Environment) -> &'v DispatchTable {
      match value {
         Value::Nil => &env.builtin_dtables.nil,
         Value::False | Value::True => &env.builtin_dtables.boolean,
         Value::Number(_) => &env.builtin_dtables.number,
         Value::String(_) => &env.builtin_dtables.string,
         Value::Function(_) => &env.builtin_dtables.function,
         Value::Struct(st) => st.dtable(),
         Value::UserData(u) => &u.dtable,
      }
   }

   /// Initializes a dispatch table with methods obtained from a method ID to function ID map.
   /// Each function's name is prepended with `type_name.`.
   fn initialize_dtable(
      &mut self,
      methods: impl Iterator<Item = (u16, Opr24)>,
      env: &mut Environment,
      dtable: &mut DispatchTable,
   ) {
      for (method_id, function_id) in methods {
         let function = unsafe { env.get_function_unchecked_mut(function_id) };
         function.name = Rc::from(format!("{}.{}", dtable.pretty_name, function.name));
         let closure = self.create_closure(env, function_id);
         dtable.set_method(method_id, closure);
      }
   }

   /// Interprets bytecode in the chunk, with the provided user state.
   pub fn interpret<S>(&mut self, mut s: S) -> Result<Value, Error>
   where
      S: UserState,
   {
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
                     return Err(self.error(s.env(), kind));
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
               let closure = self.create_closure(s.env_mut(), function_id);
               self.push(Value::Function(closure));
            }
            Opcode::CreateType => {
               let name = unsafe { self.chunk.read_string(&mut self.pc) };
               let dispatch_table = DispatchTable::new_for_type(name);
               let struct_v = Struct::new_type(Rc::new(dispatch_table));
               self.stack.push(Value::Struct(Rc::new(struct_v)));
            }
            Opcode::CreateStruct(field_count) => {
               // SAFETY: It's ok to use `unwrap_unchecked` because we're guaranteed the value on
               // top is a struct.
               //  - Constructors can only be created on types that weren't implemented yet.
               //  - Thus, the only types that can be implemented are user-defined types.
               //  - And each user-defined type is a struct.
               //  - `Opcode::Implement` aborts execution if it isn't.
               let type_struct = unsafe { self.pop().struct_v().cloned().unwrap_unchecked() };
               let field_count = u32::from(field_count) as usize;
               let instance = Rc::new(unsafe { type_struct.new_instance(field_count) });
               self.push(Value::Struct(instance));
            }

            Opcode::AssignGlobal(slot) => {
               let value = self.stack_top().clone();
               s.globals_mut().set(slot, value);
            }
            Opcode::GetGlobal(slot) => {
               let value = unsafe { s.globals().get_unchecked(slot) };
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
            Opcode::AssignUpvalue(index) => {
               let index = u32::from(index) as usize;
               let closure = unsafe { self.closure.as_ref().unwrap_unchecked() };
               let value = self.stack_top().clone();
               unsafe { Upvalue::set(&closure.captures[index], value) }
            }
            Opcode::GetUpvalue(index) => {
               let index = u32::from(index) as usize;
               let closure = unsafe { self.closure.as_ref().unwrap_unchecked() };
               let value = unsafe { closure.captures[index].get() }.clone();
               self.push(value);
            }
            Opcode::CloseLocal(stack_slot) => {
               let stack_slot = self.stack_bottom as u32 + u32::from(stack_slot);
               // This is O(n) and I can't say I'm a fan of that, but I haven't benchmarked the
               // performance impact this makes yet.
               let index =
                  self.open_upvalues.iter().rposition(|(slot, _)| *slot == stack_slot).unwrap();
               let (_, upvalue) = self.open_upvalues.remove(index);
               unsafe { upvalue.close() };
            }
            Opcode::GetField(index) => {
               let struct_v = self.pop();
               let struct_v = unsafe { struct_v.struct_v().unwrap_unchecked() };
               let value = unsafe { struct_v.get_field(u32::from(index) as usize) };
               self.push(value.clone());
            }
            Opcode::AssignField(index) => {
               let struct_v = self.pop();
               let value = self.pop();
               let struct_v = unsafe { struct_v.struct_v().unwrap_unchecked() };
               self.push(value.clone());
               unsafe { struct_v.set_field(u32::from(index) as usize, value) }
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
               // Add 1 to count in the called function itself, which is treated like an argument.
               let argument_count = argument_count as usize + 1;
               let function = self.nth_from_top(argument_count);
               let closure = Rc::clone(wrap_error!(function.function()));
               self.enter_function(&mut s, closure, argument_count)?;
            }
            Opcode::CallMethod(arg) => {
               let (method_index, argument_count) = arg.unpack();
               let receiver = self.nth_from_top(argument_count as usize);
               let dtable = Self::get_dispatch_table(receiver, s.env());
               if let Some(closure) = dtable.get_method(method_index) {
                  let closure = Rc::clone(closure);
                  self.enter_function(&mut s, closure, argument_count as usize)?;
               } else {
                  let signature =
                     s.env().get_function_signature(method_index).cloned().unwrap_or_else(|| {
                        FunctionSignature {
                           name: Rc::from("(invalid method index)"),
                           arity: None,
                        }
                     });
                  let error_kind = ErrorKind::MethodDoesNotExist {
                     type_name: Rc::clone(&dtable.pretty_name),
                     signature: FunctionSignature {
                        // Subtract 1 to omit the receiver from error messages.
                        arity: signature.arity.map(|x| x - 1),
                        ..signature
                     },
                  };
                  return Err(self.error_outside_function_call(None, s.env_mut(), error_kind));
               }
            }
            Opcode::Return => {
               let result = self.pop();
               self.restore_return_point();
               self.push(result);
            }

            Opcode::Implement(proto_id) => {
               let st = wrap_error!(self.stack_top_mut().struct_v());
               let type_name = Rc::clone(&st.dtable().type_name);
               let proto = unsafe { s.env_mut().take_prototype_unchecked(proto_id) };

               let mut type_dtable = DispatchTable::new_for_type(Rc::clone(&type_name));
               self.initialize_dtable(
                  proto.statics.iter().map(|(&k, &v)| (k, v)),
                  s.env_mut(),
                  &mut type_dtable,
               );

               let mut instance_dtable = DispatchTable::new_for_instance(type_name);
               self.initialize_dtable(
                  proto.instance.iter().map(|(&k, &v)| (k, v)),
                  s.env_mut(),
                  &mut instance_dtable,
               );

               let instance_dtable = Rc::new(instance_dtable);
               type_dtable.instance = Some(instance_dtable);
               let type_dtable = Rc::new(type_dtable);

               unsafe {
                  // Need to borrow here a second time. By this point we know that the unwrap
                  // will succeed so it's safe to use unwrap_unchecked.
                  let st = self.stack_top_mut().struct_v().unwrap_unchecked();
                  st.implement(type_dtable)
                     .map_err(|kind| self.error_outside_function_call(None, s.env_mut(), kind))?;
               }
            }

            Opcode::Negate => {
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

            Opcode::Halt => {
               self.halted = true;
               break;
            }

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

/// User-defined state that contains the environment and globals.
pub trait UserState {
   /// Returns an immutable reference to the environment.
   fn env(&self) -> &Environment;

   /// Returns an immutable reference to globals.
   fn globals(&self) -> &Globals;

   /// Returns a mutable reference to the environment.
   fn env_mut(&mut self) -> &mut Environment;

   /// Returns a mutable reference to globals.
   fn globals_mut(&mut self) -> &mut Globals;
}
