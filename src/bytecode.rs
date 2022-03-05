use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::size_of;

use bytemuck::{Pod, Zeroable};

use crate::common::{ErrorKind, Location};

/// A 24-bit integer encoding an instruction operand.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Opr24 {
   bytes: [u8; 3],
}

/// A u32 is too big to fit in an `Opr24`.
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

/// A VM opcode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
   /// Doesn't do anything. Used as a default zero value if something goes wrong.
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
   /// Assigns the value at the top of the stack to a global. The value stays on the stack.
   AssignGlobal(Opr24),
   /// Loads a value from a global.
   GetGlobal(Opr24),
   /// Assigns the value at the top of the stack to a local. The value stays on the stack.
   AssignLocal(Opr24),
   /// Loads a value from a local.
   GetLocal(Opr24),
   /// Swaps the two values at the top of the stack.
   Swap,
   /// Removes the value at the top of the stack.
   Discard,

   // Note that due to how the VM increments the program counter, forward jump instructions as if 4
   // bytes were jumped over implicitly (the actual number of bytes that is jumped over is
   // `operand + 4`).
   /// Jumps the program counter forward by an amount of bytes.
   JumpForward(Opr24),
   /// Jumps the program counter forward by an amount of bytes if the value at the top of the stack
   /// is `false`.
   JumpForwardIfFalsy(Opr24),
   /// Jumps the program counter backward by an amount of bytes.
   /// Due to how the VM increments the program counter, the actual amount is `operand - 4`.
   JumpBackward(Opr24),

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

   /// Padding value, used to make sure the enum is 4 bytes large.
   #[doc(hidden)]
   __Padding([u8; 3]),
}

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
   pub fn new() -> Self {
      Self {
         bytes: Vec::new(),
         locations: Vec::new(),
         codegen_location: Location::UNINIT,
         preallocate_stack_slots: 0,
      }
   }

   /// Pushes an opcode into the chunk. Returns where the opcode is located.
   pub fn push(&mut self, opcode: Opcode) -> usize {
      let position = self.bytes.len();
      self.bytes.extend_from_slice(opcode.as_bytes());
      self.locations.push(self.codegen_location);
      position
   }

   /// Pushes a number into the chunk.
   pub fn push_number(&mut self, number: f64) {
      let bytes = bytemuck::bytes_of(&number);
      self.bytes.extend_from_slice(bytes);
      // 8 bytes, so push twice.
      self.locations.push(self.codegen_location);
      self.locations.push(self.codegen_location);
   }

   /// Pushes a string into the chunk.
   ///
   /// The string is padded with zeroes such that opcodes are aligned to four bytes.
   pub fn push_string(&mut self, string: &str) {
      // I don't know of any 128-bit targets so this should be OK. Also, it isn't physically
      // possible to store a string as large as 2^64 bytes.
      let len = string.len() as u64;
      let len_bytes: [u8; 8] = len.to_le_bytes();
      self.bytes.extend_from_slice(&len_bytes);
      self.bytes.extend(string.as_bytes());
      let padding = ((string.len() + 3) & !3) - string.len();
      for _ in 0..padding {
         self.bytes.push(0);
      }
   }

   /// Patches the instruction at the given position.
   pub fn patch(&mut self, position: usize, opcode: Opcode) {
      let bytes = bytemuck::bytes_of(&opcode);
      self.bytes[position..position + Opcode::SIZE].copy_from_slice(bytes);
   }

   /// Reads an opcode.
   pub fn read_opcode(&self, pc: &mut usize) -> Opcode {
      let opcode = *bytemuck::from_bytes(&self.bytes[*pc..*pc + Opcode::SIZE]);
      *pc += Opcode::SIZE;
      opcode
   }

   /// Reads a number.
   pub fn read_number(&self, pc: &mut usize) -> f64 {
      const SIZE: usize = std::mem::size_of::<f64>();
      let bytes: [u8; SIZE] = self.bytes[*pc..*pc + SIZE].try_into().unwrap();
      let number = *bytemuck::from_bytes(&bytes);
      *pc += SIZE;
      number
   }

   /// Reads a string. This assumes the original string was encoded as proper UTF-8 (which it should
   /// have been considering the only way to write a string is to use a `&str` in the first place).
   pub fn read_string(&self, pc: &mut usize) -> &str {
      let len_bytes: [u8; 8] = self.bytes[*pc..*pc + size_of::<u64>()].try_into().unwrap();
      let len = u64::from_le_bytes(len_bytes);
      *pc += size_of::<u64>();
      let string = &self.bytes[*pc..*pc + len as usize];
      let padded_len = (len + 3) & !3;
      *pc += padded_len as usize;
      unsafe { std::str::from_utf8_unchecked(string) }
   }

   /// Returns the length of the chunk (in bytes).
   pub fn len(&self) -> usize {
      self.bytes.len()
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
      let mut pc = 0;
      while !self.at_end(pc) {
         let opcode = self.read_opcode(&mut pc);
         write!(f, "{pc:08x} {opcode:?} ")?;

         #[allow(clippy::single_match)]
         match opcode {
            Opcode::PushNumber => write!(f, "{}", self.read_number(&mut pc))?,
            Opcode::PushString => write!(f, "{:?}", self.read_string(&mut pc))?,
            _ => (),
         }

         writeln!(f)?;
      }
      Ok(())
   }
}

/// An environment containing information about declared globals.
#[derive(Debug, Clone)]
pub struct GlobalInfo {
   /// Mapping from global names to global slots.
   globals: HashMap<String, Opr24>,
}

impl GlobalInfo {
   /// Creates a new, empty environment.
   pub fn new() -> Self {
      Self {
         globals: HashMap::new(),
      }
   }

   /// Tries to create a global. Returns the global slot number, or an error if there are too many
   /// globals.
   pub fn create(&mut self, name: &str) -> Result<Opr24, ErrorKind> {
      let slot = Opr24::new(self.globals.len().try_into().unwrap())
         .map_err(|_| ErrorKind::TooManyGlobals)?;
      self.globals.insert(name.to_owned(), slot);
      Ok(slot)
   }

   /// Tries to look up a global. Returns `None` if the global doesn't exist.
   pub fn get(&mut self, name: &str) -> Option<Opr24> {
      self.globals.get(name).copied()
   }
}
