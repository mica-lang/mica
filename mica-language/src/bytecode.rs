use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::mem::size_of;
use std::rc::Rc;

use bytemuck::{Pod, Zeroable};

use crate::common::{ErrorKind, Location};
use crate::value::{Closure, Value};

/// A 24-bit integer encoding an instruction operand.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Opr24 {
   bytes: [u8; 3],
}

/// A u32 is too big to fit in an `Opr24`.
#[derive(Debug)]
pub struct U32TooBig(());

impl Opr24 {
   pub const MAX: u32 = (1 << 24);

   /// Tries to construct a new `Opr24`.
   pub fn new(x: u32) -> Result<Self, U32TooBig> {
      if x < Self::MAX {
         Ok(Self {
            bytes: [
               (x & 0xFF) as u8,
               ((x >> 8) & 0xFF) as u8,
               ((x >> 16) & 0xFF) as u8,
            ],
         })
      } else {
         Err(U32TooBig(()))
      }
   }

   pub fn pack<T>(x: T) -> Self
   where
      T: PackableToOpr24,
   {
      x.pack_to_opr24()
   }

   pub fn unpack<T>(self) -> T
   where
      T: PackableToOpr24,
   {
      T::unpack_from_opr24(self)
   }
}

impl TryFrom<usize> for Opr24 {
   type Error = U32TooBig;

   fn try_from(value: usize) -> Result<Self, Self::Error> {
      Self::new(u32::try_from(value).map_err(|_| U32TooBig(()))?)
   }
}

impl From<Opr24> for u32 {
   fn from(opr: Opr24) -> u32 {
      (opr.bytes[0] as u32) | ((opr.bytes[1] as u32) << 8) | ((opr.bytes[2] as u32) << 16)
   }
}

impl std::fmt::Debug for Opr24 {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{:x}", u32::from(*self))
   }
}

impl std::fmt::Display for Opr24 {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{:?}", self)
   }
}

unsafe impl Zeroable for Opr24 {}
unsafe impl Pod for Opr24 {}

pub trait PackableToOpr24 {
   fn pack_to_opr24(self) -> Opr24;
   fn unpack_from_opr24(opr: Opr24) -> Self;
}

impl PackableToOpr24 for (u16, u8) {
   fn pack_to_opr24(self) -> Opr24 {
      // SAFETY: This packs the two numbers into 24 bits and will never exceed the range of an
      // Opr24.
      unsafe { Opr24::new((self.0 as u32) << 8 | self.1 as u32).unwrap_unchecked() }
   }

   fn unpack_from_opr24(opr: Opr24) -> Self {
      let x = u32::from(opr);
      let big = ((x & 0xFFFF00) >> 8) as u16;
      let small = (x & 0x0000FF) as u8;
      (big, small)
   }
}

/// A VM opcode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
   /// Doesn't do anything. Used as a default zero value if something goes wrong.
   /// Also used for backpatching purposes.
   #[allow(unused)]
   Nop,

   /// Pushes `nil` onto the stack.
   PushNil,
   /// Pushes `true` onto the stack.
   PushTrue,
   /// Pushes `false` onto the stack.
   PushFalse,
   /// Pushes a number onto the stack. Must be followed by an f64.
   PushNumber,
   /// Pushes a string onto the stack. Must be followed by a string.
   PushString,
   /// Creates a closure from the function with the given ID and pushes it onto the stack.
   CreateClosure(Opr24),
   /// Creates a unique type that can be later implemented. Must be followed by a string indicating
   /// the type's name.
   CreateType,
   /// Creates a struct instance from the type at the top of the stack, with the specified amount
   /// of fields.
   CreateStruct(Opr24),

   /// Assigns the value at the top of the stack to a global. The value stays on the stack.
   AssignGlobal(Opr24),
   /// Loads a value from a global.
   GetGlobal(Opr24),
   /// Assigns the value at the top of the stack to a local. The value stays on the stack.
   AssignLocal(Opr24),
   /// Loads a value from a local.
   GetLocal(Opr24),
   /// Assigns the value at the top of the stack to an upvalue. The value stays on the stack.
   AssignUpvalue(Opr24),
   /// Loads a value from an upvalue.
   GetUpvalue(Opr24),
   /// Closes a local in its upvalue.
   CloseLocal(Opr24),
   /// Loads a field from the struct on the top of the stack.
   /// Assumes the value on top is a struct and not something else.
   GetField(Opr24),
   /// Assigns to a field in the struct on the top of the stack. The struct is consumed but the
   /// value remains on the stack.
   /// Assumes the second value from top is a struct and not something else.
   AssignField(Opr24),

   /// Swaps the two values at the top of the stack.
   Swap,
   /// Removes the value at the top of the stack.
   Discard,

   // Note that due to how the VM increments the program counter, forward jump instructions as if
   // 4 bytes were jumped over implicitly (the actual number of bytes that is jumped over is
   // `operand + 4`).
   /// Jumps the program counter forward by an amount of bytes.
   JumpForward(Opr24),
   /// Jumps the program counter forward by an amount of bytes if the value at the top of the stack
   /// is falsy.
   JumpForwardIfFalsy(Opr24),
   /// Jumps the program counter forward by an amount of bytes if the value at the top of the stack
   /// is truthy.
   JumpForwardIfTruthy(Opr24),
   /// Jumps the program counter backward by an amount of bytes.
   /// Due to how the VM increments the program counter, the actual amount is `operand - 4`.
   JumpBackward(Opr24),
   /// Enters a breakable block by pushing the break sentinel value onto the stack.
   EnterBreakableBlock,
   /// Exits the n-th breakable block (counted from innermost) by popping values off the stack
   /// until `.0` sentinels are removed.
   ExitBreakableBlock(u16),

   /// Calls a function with `.0` arguments.
   Call(u16),
   /// Calls the `n`th method with `a` arguments, where `a` is encoded in the lower 8 bits, and
   /// `n` is encoded in the upper 16 bits of `.0`.
   CallMethod(Opr24),
   /// Returns to the calling function.
   Return,

   /// Implements a struct according to a prototype identified by the operand.
   Implement(Opr24),

   /// Negates a number (prefix `-`).
   Negate,
   /// Adds two numbers together (infix `+`).
   Add,
   /// Subtracts a number from another number (infix `-`).
   Subtract,
   /// Multiplies two numbers together (infix `*`).
   Multiply,
   /// Divides a number by another number (infix `/`).
   Divide,

   /// Flips a boolean-like value (truthy values become `false` and falsy values become `true`).
   Not,
   /// Compares two values for equality.
   Equal,
   /// Compares two values for less-than relation.
   Less,
   /// Compares two values for less-than-or-equal relation.
   LessEqual,

   /// Halts the interpreter loop.
   Halt,

   /// Padding value, used to make sure the enum is 4 bytes large.
   #[doc(hidden)]
   __Padding([u8; 3]),
}

#[derive(Debug)]
pub struct JumpTooFar(());

impl Opcode {
   pub const SIZE: usize = {
      let size = std::mem::size_of::<Self>();
      assert!(size == 4);
      size
   };

   pub fn as_bytes(&self) -> &[u8] {
      bytemuck::bytes_of(self)
   }

   fn forward_jump_offset(from: usize, to: usize) -> Result<Opr24, JumpTooFar> {
      assert!(to >= from);
      let offset = to - from - Self::SIZE;
      if u32::try_from(offset).is_err() {
         return Err(JumpTooFar(()));
      }
      Opr24::new(offset as u32).map_err(|_| JumpTooFar(()))
   }

   pub fn jump_forward(from: usize, to: usize) -> Result<Self, JumpTooFar> {
      let offset = Self::forward_jump_offset(from, to)?;
      Ok(Self::JumpForward(offset))
   }

   pub fn jump_forward_if_falsy(from: usize, to: usize) -> Result<Self, JumpTooFar> {
      let offset = Self::forward_jump_offset(from, to)?;
      Ok(Self::JumpForwardIfFalsy(offset))
   }

   pub fn jump_forward_if_truthy(from: usize, to: usize) -> Result<Self, JumpTooFar> {
      let offset = Self::forward_jump_offset(from, to)?;
      Ok(Self::JumpForwardIfTruthy(offset))
   }

   fn backward_jump_offset(from: usize, to: usize) -> Result<Opr24, JumpTooFar> {
      assert!(to <= from);
      let offset = from - to + Self::SIZE;
      if u32::try_from(offset).is_err() {
         return Err(JumpTooFar(()));
      }
      Opr24::new(offset as u32).map_err(|_| JumpTooFar(()))
   }

   pub fn jump_backward(from: usize, to: usize) -> Result<Self, JumpTooFar> {
      let offset = Self::backward_jump_offset(from, to)?;
      Ok(Self::JumpBackward(offset))
   }
}

unsafe impl Zeroable for Opcode {}
unsafe impl Pod for Opcode {}

/// A chunk of bytecode.
pub struct Chunk {
   /// The name of the module where the chunk is located.
   pub module_name: Rc<str>,
   /// The actual bytecode.
   bytes: Vec<u8>,
   /// Locations. These are placed at multiples of four bytes (Opcode::SIZE).
   locations: Vec<Location>,
   /// The location emitted for each quad-byte on calls to `push`.
   pub codegen_location: Location,
   /// How many stack slots to preallocate with `nil` values for variable lookups.
   pub preallocate_stack_slots: u32,
}

impl Chunk {
   /// Constructs an empty chunk.
   pub fn new(module_name: Rc<str>) -> Self {
      Self {
         module_name,
         bytes: Vec::new(),
         locations: Vec::new(),
         codegen_location: Location::UNINIT,
         preallocate_stack_slots: 0,
      }
   }

   /// Pushes an opcode into the chunk. Returns where the opcode is located.
   pub fn emit(&mut self, opcode: Opcode) -> usize {
      let position = self.bytes.len();
      self.bytes.extend_from_slice(opcode.as_bytes());
      self.locations.push(self.codegen_location);
      position
   }

   /// Pushes a number into the chunk.
   pub fn emit_number(&mut self, number: f64) {
      let bytes = number.to_le_bytes();
      self.bytes.extend_from_slice(&bytes);
      // 8 bytes, so push twice.
      self.locations.push(self.codegen_location);
      self.locations.push(self.codegen_location);
   }

   /// Pushes a string into the chunk.
   ///
   /// The string is padded with zeroes such that opcodes are aligned to four bytes.
   pub fn emit_string(&mut self, string: &str) {
      let start = self.len();

      // I don't know of any 128-bit targets so this cast should be OK. Also, it isn't physically
      // possible to store a string as large as 2^64 bytes.
      let len = string.len() as u64;
      let len_bytes: [u8; 8] = len.to_le_bytes();
      self.bytes.extend_from_slice(&len_bytes);
      self.bytes.extend(string.as_bytes());
      let padded_len = (string.len() + 3) & !3;
      let padding = padded_len - string.len();
      for _ in 0..padding {
         self.bytes.push(0);
      }

      let end = self.len();
      for _ in (0..end - start).step_by(4) {
         self.locations.push(self.codegen_location);
      }
   }

   /// Patches the instruction at the given position.
   pub fn patch(&mut self, position: usize, opcode: Opcode) {
      let bytes = bytemuck::bytes_of(&opcode);
      self.bytes[position..position + Opcode::SIZE].copy_from_slice(bytes);
   }

   /// Reads an opcode.
   ///
   /// # Safety
   /// Assumes that `pc` is within the chunk's bounds, skipping any checks.
   pub unsafe fn read_opcode(&self, pc: &mut usize) -> Opcode {
      let bytes = self.bytes.get_unchecked(*pc..*pc + Opcode::SIZE);
      let opcode = *bytemuck::from_bytes(bytes);
      *pc += Opcode::SIZE;
      opcode
   }

   /// Reads a number.
   ///
   /// # Safety
   /// Assumes that `pc` is within the chunk's bounds, skipping any checks.
   pub unsafe fn read_number(&self, pc: &mut usize) -> f64 {
      const SIZE: usize = std::mem::size_of::<f64>();
      let bytes = self.bytes.get_unchecked(*pc..*pc + SIZE);
      let bytes: [u8; SIZE] = bytes.try_into().unwrap_unchecked();
      let number = f64::from_le_bytes(bytes);
      *pc += SIZE;
      number
   }

   /// Reads a string.
   ///
   /// # Safety
   /// This assumes the original string was encoded as proper UTF-8 (which it should
   /// have been considering the only way to write a string is to use a `&str` in the first place).
   pub unsafe fn read_string(&self, pc: &mut usize) -> &str {
      let len_bytes: [u8; 8] = self.bytes[*pc..*pc + size_of::<u64>()].try_into().unwrap();
      let len = u64::from_le_bytes(len_bytes);
      *pc += size_of::<u64>();
      let string = &self.bytes[*pc..*pc + len as usize];
      let padded_len = (len + 3) & !3;
      *pc += padded_len as usize;
      std::str::from_utf8_unchecked(string)
   }

   /// Returns the length of the chunk (in bytes).
   pub fn len(&self) -> usize {
      self.bytes.len()
   }

   /// Returns whether the chunk is empty.
   pub fn is_empty(&self) -> bool {
      self.len() == 0
   }

   /// Returns the location (in file) of the program counter.
   pub fn location(&self, pc: usize) -> Location {
      let index = pc >> 2;
      self.locations.get(index).copied().unwrap_or(Location::UNINIT)
   }

   /// Returns whether the given program counter is at the end of the chunk.
   pub fn at_end(&self, pc: usize) -> bool {
      pc >= self.bytes.len()
   }
}

impl Debug for Chunk {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.debug_struct("Chunk")
         .field("module_name", &self.module_name)
         .field("preallocate_stack_slots", &self.preallocate_stack_slots)
         .finish()?;
      writeln!(f)?;

      let mut pc = 0;
      while !self.at_end(pc) {
         let location = self.location(pc);
         let opcode = unsafe { self.read_opcode(&mut pc) };
         write!(f, "{pc:06x} {} {opcode:?} ", location)?;

         #[allow(clippy::single_match)]
         match opcode {
            Opcode::PushNumber => write!(f, "{}", unsafe { self.read_number(&mut pc) })?,
            Opcode::PushString | Opcode::CreateType => {
               write!(f, "{:?}", unsafe { self.read_string(&mut pc) })?
            }
            | Opcode::JumpForward(amount)
            | Opcode::JumpForwardIfFalsy(amount)
            | Opcode::JumpForwardIfTruthy(amount) => {
               write!(
                  f,
                  "-> {:06x}",
                  pc + u32::from(amount) as usize + Opcode::SIZE
               )?;
            }
            Opcode::JumpBackward(amount) => {
               write!(
                  f,
                  "-> {:06x}",
                  pc - u32::from(amount) as usize + Opcode::SIZE
               )?;
            }
            Opcode::CallMethod(arg) => {
               let (method_index, argument_count) = arg.unpack();
               let arg = u32::from(arg);
               write!(f, "{arg:06x}:[mi={method_index}, ac={argument_count}]")?;
            }
            _ => (),
         }

         writeln!(f)?;
      }
      Ok(())
   }
}

/// The kind of an upvalue capture.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CaptureKind {
   /// Capture a local from the current scope.
   Local(Opr24),
   /// Capture an existing upvalue from the current closure.
   Upvalue(Opr24),
}

/// The ABI of a raw foreign function.
pub type ForeignFunction = Box<dyn FnMut(&[Value]) -> Result<Value, ErrorKind>>;

/// The kind of the function (bytecode or FFI).
pub enum FunctionKind {
   Bytecode {
      chunk: Rc<Chunk>,
      captured_locals: Vec<CaptureKind>,
   },
   Foreign(ForeignFunction),
}

impl std::fmt::Debug for FunctionKind {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Self::Bytecode {
            chunk,
            captured_locals,
         } => f
            .debug_struct("Bytecode")
            .field("chunk", chunk)
            .field("captured_locals", captured_locals)
            .finish(),
         Self::Foreign(..) => f.debug_struct("Foreign").finish_non_exhaustive(),
      }
   }
}

/// A function prototype.
#[derive(Debug)]
pub struct Function {
   pub name: Rc<str>,
   pub parameter_count: Option<u16>,
   pub kind: FunctionKind,
}

/// The signature of a function (its name and argument count).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
   pub name: Rc<str>,
   /// This arity number does not include the implicit `self` argument.
   pub arity: Option<u16>,
}

impl fmt::Display for FunctionSignature {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      if let Some(arity) = self.arity {
         write!(f, "{}/{}", self.name, arity)
      } else {
         write!(f, "{}/...", self.name)
      }
   }
}

/// An environment containing information about declared globals, functions, vtables.
#[derive(Debug)]
pub struct Environment {
   /// Mapping from global names to global slots.
   globals: HashMap<String, Opr24>,
   /// Functions in the environment.
   functions: Vec<Function>,
   /// Mapping from named function signatures to method indices.
   method_indices: HashMap<FunctionSignature, u16>,
   /// Mapping from method indices to function signatures.
   function_signatures: Vec<FunctionSignature>,
   /// Dispatch tables for builtin types.
   pub builtin_dtables: BuiltinDispatchTables,
   /// `impl` prototypes.
   prototypes: Vec<Option<Prototype>>,
   /// `prototypes` indices that have been taken out of the vec.
   free_prototypes: Vec<Opr24>,
}

impl Environment {
   /// Creates a new, empty environment.
   pub fn new(builtin_dtables: BuiltinDispatchTables) -> Self {
      Self {
         globals: HashMap::new(),
         functions: Vec::new(),
         method_indices: HashMap::new(),
         function_signatures: Vec::new(),
         builtin_dtables,
         prototypes: Vec::new(),
         free_prototypes: Vec::new(),
      }
   }

   /// Tries to create a global. Returns the global slot number, or an error if there are too many
   /// globals.
   pub fn create_global(&mut self, name: &str) -> Result<Opr24, ErrorKind> {
      let slot = Opr24::new(self.globals.len().try_into().map_err(|_| ErrorKind::TooManyGlobals)?)
         .map_err(|_| ErrorKind::TooManyGlobals)?;
      self.globals.insert(name.to_owned(), slot);
      Ok(slot)
   }

   /// Tries to look up a global. Returns `None` if the global doesn't exist.
   pub fn get_global(&mut self, name: &str) -> Option<Opr24> {
      self.globals.get(name).copied()
   }

   /// Creates a function and returns its ID.
   pub fn create_function(&mut self, function: Function) -> Result<Opr24, ErrorKind> {
      let slot = Opr24::try_from(self.functions.len()).map_err(|_| ErrorKind::TooManyFunctions)?;
      self.functions.push(function);
      Ok(slot)
   }

   /// Returns the function with the given ID, as returned by `create_function`.
   /// This function is for internal use in the VM and does not perform any checks, thus is marked
   /// `unsafe`.
   pub(crate) unsafe fn get_function_unchecked(&self, id: Opr24) -> &Function {
      self.functions.get_unchecked(u32::from(id) as usize)
   }

   /// Returns the function with the given ID, as returned by `create_function`.
   /// This function is for internal use in the VM and does not perform any checks, thus is marked
   /// `unsafe`.
   pub(crate) unsafe fn get_function_unchecked_mut(&mut self, id: Opr24) -> &mut Function {
      self.functions.get_unchecked_mut(u32::from(id) as usize)
   }

   /// Tries to look up the index of a method, based on a function signature. Returns `None` if
   /// there are too many function signatures in this environment.
   pub fn get_method_index(&mut self, signature: &FunctionSignature) -> Result<u16, ErrorKind> {
      // Don't use `entry` here to avoid cloning the signature.
      if let Some(&index) = self.method_indices.get(signature) {
         Ok(index)
      } else {
         // The number of entries in self.method_indices and self.function_signatures is always
         // equal, so we can use their `len`s interchangably.
         let index =
            u16::try_from(self.method_indices.len()).map_err(|_| ErrorKind::TooManyMethods)?;
         self.method_indices.insert(signature.clone(), index);
         self.function_signatures.push(signature.clone());
         Ok(index)
      }
   }

   /// Returns the function with the given signature, or `None` if the method index is invalid.
   pub fn get_function_signature(&self, method_index: u16) -> Option<&FunctionSignature> {
      self.function_signatures.get(method_index as usize)
   }

   /// Creates a prototype and returns its ID.
   pub(crate) fn create_prototype(&mut self, proto: Prototype) -> Result<Opr24, ErrorKind> {
      let slot = if let Some(slot) = self.free_prototypes.pop() {
         self.prototypes[u32::from(slot) as usize] = Some(proto);
         slot
      } else {
         let slot = Opr24::try_from(self.prototypes.len()).map_err(|_| ErrorKind::TooManyImpls)?;
         self.prototypes.push(Some(proto));
         slot
      };
      Ok(slot)
   }

   /// Takes out the prototype with the given ID, as returned by `create_prototype`.
   /// This function is for internal use in the VM and does not perform any checks, thus is marked
   /// `unsafe`.
   pub(crate) unsafe fn take_prototype_unchecked(&mut self, id: Opr24) -> Prototype {
      let proto =
         self.prototypes.get_unchecked_mut(u32::from(id) as usize).take().unwrap_unchecked();
      self.free_prototypes.push(id);
      proto
   }
}

/// A dispatch table containing functions bound to an instance of a value.
#[derive(Debug)]
pub struct DispatchTable {
   /// The pretty name of the type this dispatch table contains functions for.
   pub pretty_name: Rc<str>,
   pub type_name: Rc<str>,
   /// The "child" dispatch table that holds instance methods.
   pub instance: Option<Rc<DispatchTable>>,
   /// The functions in this dispatch table.
   methods: Vec<Option<Rc<Closure>>>,
}

impl DispatchTable {
   /// Creates a new, empty dispatch table for a type with the given name.
   fn new(pretty_name: impl Into<Rc<str>>, type_name: impl Into<Rc<str>>) -> Self {
      Self {
         pretty_name: pretty_name.into(),
         type_name: type_name.into(),
         instance: None,
         methods: Vec::new(),
      }
   }

   /// Creates a new, empty type dispatch table with the given type name.
   pub fn new_for_type(type_name: impl Into<Rc<str>>) -> Self {
      let type_name = type_name.into();
      Self::new(format!("type {type_name}"), type_name)
   }

   /// Creates a new, empty instance dispatch table with the given type name.
   pub fn new_for_instance(type_name: impl Into<Rc<str>>) -> Self {
      let type_name = type_name.into();
      Self::new(Rc::clone(&type_name), type_name)
   }

   /// Returns a reference to the method at the given index.
   pub fn get_method(&self, index: u16) -> Option<&Rc<Closure>> {
      self.methods.get(index as usize).into_iter().flatten().next()
   }

   /// Adds a method into the dispatch table.
   pub fn set_method(&mut self, index: u16, closure: Rc<Closure>) {
      let index = index as usize;
      if index >= self.methods.len() {
         self.methods.resize(index + 1, None);
      }
      self.methods[index] = Some(closure);
   }
}

/// Dispatch tables for instances of builtin types. These should be constructed by the standard
/// library.
#[derive(Debug)]
pub struct BuiltinDispatchTables {
   pub nil: Rc<DispatchTable>,
   pub boolean: Rc<DispatchTable>,
   pub number: Rc<DispatchTable>,
   pub string: Rc<DispatchTable>,
   pub function: Rc<DispatchTable>,
}

/// Default dispatch tables for built-in types are empty and do not implement any methods.
impl Default for BuiltinDispatchTables {
   fn default() -> Self {
      Self {
         nil: Rc::new(DispatchTable::new("Nil", "Nil")),
         boolean: Rc::new(DispatchTable::new("Boolean", "Boolean")),
         number: Rc::new(DispatchTable::new("Number", "Boolean")),
         string: Rc::new(DispatchTable::new("String", "String")),
         function: Rc::new(DispatchTable::new("Function", "Function")),
      }
   }
}

/// The prototype of a struct. This contains a list of functions, from which closures are
/// constructed at runtime to form a dispatch table.
#[derive(Debug, Default)]
pub(crate) struct Prototype {
   pub(crate) instance: HashMap<u16, Opr24>,
   pub(crate) statics: HashMap<u16, Opr24>,
}
