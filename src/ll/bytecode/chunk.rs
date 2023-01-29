//! Chunks of bytecode.

use std::{fmt, mem::size_of, rc::Rc};

use super::{EncodeInstruction, Opcode, Opr24};
use crate::ll::error::Location;

/// A chunk of bytecode.
pub struct Chunk {
    /// The name of the module where the chunk is located.
    pub module_name: Rc<str>,
    /// The actual bytecode.
    bytes: Vec<u8>,
    /// Locations. These are placed at multiples of four bytes (Opcode::INSTRUCTION_SIZE).
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

    /// Pushes an encodable piece of data into the chunk. Returns where it's located.
    pub fn emit(&mut self, instruction: impl EncodeInstruction) -> usize {
        let position = self.bytes.len();
        self.bytes.extend_from_slice(&instruction.encode_instruction());
        // Only push one location, since all encoded instructions are 4 bytes long.
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
    pub fn patch(&mut self, position: usize, instruction: impl EncodeInstruction) {
        let bytes = instruction.encode_instruction();
        self.bytes[position..position + Opcode::INSTRUCTION_SIZE].copy_from_slice(&bytes);
    }

    /// Reads an instruction.
    ///
    /// # Safety
    /// Assumes that `pc` is within the chunk's bounds and that the opcode at `pc` is valid.
    pub unsafe fn read_instruction(&self, pc: &mut usize) -> (Opcode, Opr24) {
        let bytes = &self.bytes[*pc..*pc + Opcode::INSTRUCTION_SIZE];
        let mut bytes = <[u8; Opcode::INSTRUCTION_SIZE]>::try_from(bytes).unwrap();
        let opcode: Opcode = std::mem::transmute(bytes[0]);
        bytes[0] = 0;
        let operand = Opr24 { bytes: [bytes[1], bytes[2], bytes[3]] };
        *pc += Opcode::INSTRUCTION_SIZE;
        (opcode, operand)
    }

    /// Reads a number.
    ///
    /// # Safety
    /// Assumes that `pc` is within the chunk's bounds, skipping any checks.
    pub unsafe fn read_number(&self, pc: &mut usize) -> f64 {
        const SIZE: usize = std::mem::size_of::<f64>();
        let bytes = &self.bytes[*pc..*pc + SIZE];
        let bytes: [u8; SIZE] = bytes.try_into().unwrap();
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

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Chunk")
            .field("module_name", &self.module_name)
            .field("preallocate_stack_slots", &self.preallocate_stack_slots)
            .finish()?;
        writeln!(f)?;

        let mut pc = 0;
        while !self.at_end(pc) {
            let location = self.location(pc);
            let show_pc = pc;
            let (opcode, operand) = unsafe { self.read_instruction(&mut pc) };
            write!(f, "{show_pc:06x} {location} {opcode:?}({operand:?}) ")?;

            #[allow(clippy::single_match)]
            match opcode {
                Opcode::PushNumber => write!(f, "{}", unsafe { self.read_number(&mut pc) })?,
                Opcode::PushString | Opcode::CreateType => {
                    write!(f, "{:?}", unsafe { self.read_string(&mut pc) })?
                }
                Opcode::JumpForward | Opcode::JumpForwardIfFalsy | Opcode::JumpForwardIfTruthy => {
                    write!(
                        f,
                        "-> {:06x}",
                        pc + u32::from(operand) as usize + Opcode::INSTRUCTION_SIZE
                    )?;
                }
                Opcode::JumpBackward => {
                    write!(
                        f,
                        "-> {:06x}",
                        pc - u32::from(operand) as usize + Opcode::INSTRUCTION_SIZE
                    )?;
                }
                Opcode::CallMethod => {
                    let (method_index, argument_count) = operand.unpack();
                    let operand = u32::from(operand);
                    write!(f, "{operand:06x}:[mi={method_index}, ac={argument_count}]")?;
                }
                _ => (),
            }

            writeln!(f)?;
        }
        Ok(())
    }
}
