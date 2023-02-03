//! Opcodes and the encoding of instructions into bytes.

use super::Opr24;

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
    CreateClosure,
    /// Creates a unique type that can be later implemented. Must be followed by a string
    /// indicating the type's name.
    CreateType,
    /// Creates a struct instance from the type at the top of the stack, with the specified amount
    /// of fields.
    CreateStruct,
    /// Creates an instance of the trait with the given ID and pushes it onto the stack.
    CreateTrait,
    /// Creates a list from `operand` values that are at the top of the stack.
    CreateList,
    /// Creates a dict from `operand * 2` values that are at the top of the stack. The values have
    /// to be arranged in `key, value, key, value...` order, from bottom to top.
    CreateDict,
    /// Creates a tuple from `operand` values that are at the top of the stack.
    CreateTuple,

    /// Assigns the value at the top of the stack to a global. The value stays on the stack.
    AssignGlobal,
    /// Sinks the value at the top of the stack to a global. The value is consumed.
    SinkGlobal,
    /// Loads a value from a global.
    GetGlobal,
    /// Assigns the value at the top of the stack to a local. The value stays on the stack.
    AssignLocal,
    /// Sinks the value at the top of the stack to a local. The value is consumed.
    SinkLocal,
    /// Loads a value from a local.
    GetLocal,
    /// Assigns the value at the top of the stack to an upvalue. The value stays on the stack.
    AssignUpvalue,
    /// Sinks the value at the top of the stack to an upvalue. The value is consumed.
    SinkUpvalue,
    /// Loads a value from an upvalue.
    GetUpvalue,
    /// Closes a local in its upvalue.
    CloseLocal,
    /// Assigns to a field in the struct on the top of the stack. The struct is consumed but the
    /// value remains on the stack.
    /// Assumes the second value from top is a struct and not something else.
    AssignField,
    /// Sinks to a field in the struct on the top of the stack. Both the struct and the value are
    /// consumed.
    SinkField,
    /// Loads a field from the struct on the top of the stack.
    /// Assumes the value on top is a struct and not something else.
    GetField,

    /// Destructures a tuple at the top of the stack into its individual components. The operand
    /// signifies how many elements the tuple must have to be successfully destructured; if the
    /// tuple has a different size, an error is thrown.
    DestructureTuple,

    /// Swaps the two values at the top of the stack.
    Swap,
    /// Duplicates the value at the top of the stack.
    Duplicate,
    /// Removes the value at the top of the stack.
    Discard,

    // Note that due to how the VM increments the program counter, forward jump instructions as if
    // 4 bytes were jumped over implicitly (the actual number of bytes that is jumped over is
    // `operand + 4`).
    /// Jumps the program counter forward by an amount of bytes.
    JumpForward,
    /// Jumps the program counter forward by an amount of bytes if the value at the top of the
    /// stack is falsy.
    JumpForwardIfFalsy,
    /// Jumps the program counter forward by an amount of bytes if the value at the top of the
    /// stack is truthy.
    JumpForwardIfTruthy,
    /// Jumps the program counter backward by an amount of bytes.
    /// Due to how the VM increments the program counter, the actual amount is `operand - 4`.
    JumpBackward,
    /// Enters a breakable block by pushing the break sentinel value onto the stack.
    EnterBreakableBlock,
    /// Exits the n-th breakable block (counted from innermost) by popping values off the stack
    /// until `.0` sentinels are removed.
    ExitBreakableBlock,

    /// Calls a function with `.0` arguments.
    Call,
    /// Calls the `n`th method with `a` arguments, where `a` is encoded in the lower 8 bits, and
    /// `n` is encoded in the upper 16 bits of `.0`.
    CallMethod,
    /// Returns to the calling function.
    Return,

    /// Implements a struct according to a prototype identified by the operand.
    Implement,

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
}

/// A jump was constructed whose offset stretched too far.
#[derive(Debug)]
pub struct JumpTooFar(());

impl Opcode {
    /// The size of an instruction (1 byte opcode + 3 bytes operand).
    pub const INSTRUCTION_SIZE: usize = 4;

    /// Returns the offset of a forward jump instruction.
    fn forward_jump_offset(from: usize, to: usize) -> Result<Opr24, JumpTooFar> {
        assert!(to >= from);
        let offset = to - from - Self::INSTRUCTION_SIZE;
        if u32::try_from(offset).is_err() {
            return Err(JumpTooFar(()));
        }
        Opr24::new(offset as u32).map_err(|_| JumpTooFar(()))
    }

    /// Constructs a `JumpForward` instruction.
    pub fn jump_forward(from: usize, to: usize) -> Result<(Self, Opr24), JumpTooFar> {
        let offset = Self::forward_jump_offset(from, to)?;
        Ok((Self::JumpForward, offset))
    }

    /// Constructs a `JumpForwardIfFalsy` instruction.
    pub fn jump_forward_if_falsy(from: usize, to: usize) -> Result<(Self, Opr24), JumpTooFar> {
        let offset = Self::forward_jump_offset(from, to)?;
        Ok((Self::JumpForwardIfFalsy, offset))
    }

    /// Constructs a `JumpForwardIfTruthy` instruction.
    pub fn jump_forward_if_truthy(from: usize, to: usize) -> Result<(Self, Opr24), JumpTooFar> {
        let offset = Self::forward_jump_offset(from, to)?;
        Ok((Self::JumpForwardIfTruthy, offset))
    }

    /// Returns the offset of a backward jump instruction.
    fn backward_jump_offset(from: usize, to: usize) -> Result<Opr24, JumpTooFar> {
        assert!(to <= from);
        let offset = from - to + Self::INSTRUCTION_SIZE;
        if u32::try_from(offset).is_err() {
            return Err(JumpTooFar(()));
        }
        Opr24::new(offset as u32).map_err(|_| JumpTooFar(()))
    }

    /// Constructs a `JumpBackward` instruction.
    pub fn jump_backward(from: usize, to: usize) -> Result<(Self, Opr24), JumpTooFar> {
        let offset = Self::backward_jump_offset(from, to)?;
        Ok((Self::JumpBackward, offset))
    }
}

/// Types that can be encoded into bytecode.
pub trait EncodeInstruction {
    fn encode_instruction(&self) -> [u8; Opcode::INSTRUCTION_SIZE];
}

impl EncodeInstruction for (Opcode, Opr24) {
    fn encode_instruction(&self) -> [u8; Opcode::INSTRUCTION_SIZE] {
        [self.0 as u8, self.1.bytes[0], self.1.bytes[1], self.1.bytes[2]]
    }
}

impl EncodeInstruction for (Opcode, u16) {
    fn encode_instruction(&self) -> [u8; Opcode::INSTRUCTION_SIZE] {
        let ubytes = self.1.to_le_bytes();
        [self.0 as u8, ubytes[0], ubytes[1], 0]
    }
}

impl EncodeInstruction for Opcode {
    fn encode_instruction(&self) -> [u8; Opcode::INSTRUCTION_SIZE] {
        (*self, Opr24 { bytes: [0, 0, 0] }).encode_instruction()
    }
}
