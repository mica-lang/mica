//! Low-level operations on variables and scopes.

use std::collections::HashMap;

use super::{CodeGenerator, ExpressionResult};
use crate::ll::{
    ast::{Ast, NodeId},
    bytecode::{CaptureKind, GlobalIndex, Opcode, Opr24},
    error::{Error, ErrorKind},
};

/// The index of a local on the stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct LocalIndex(Opr24);

impl LocalIndex {
    pub(crate) fn to_u32(self) -> u32 {
        u32::from(self.0)
    }
}

/// The index of an upvalue in a closure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct UpvalueIndex(Opr24);

impl UpvalueIndex {
    pub(crate) fn to_u32(self) -> u32 {
        u32::from(self.0)
    }
}

/// Info about a local variable on the stack.
#[derive(Debug)]
struct Variable {
    stack_slot: LocalIndex,
    is_captured: bool,
}

#[derive(Debug, Default)]
struct Scope {
    /// Mapping from variable names to stack slots.
    variables_by_name: HashMap<String, Variable>,
    allocated_variable_count: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum VariablePlace {
    Global(GlobalIndex),
    Local(LocalIndex),
    Upvalue(UpvalueIndex),
}

/// The kind of a variable allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum VariableAllocation {
    /// Inherit the allocation from the caller (parameter passing).
    Inherit,
    /// Allocate new storage for the variable.
    Allocate,
}

/// Local variables, including upvalues.
#[derive(Default)]
pub(super) struct Locals {
    /// If there is a parent code generator with its own scopes (the current instance is in the
    /// middle of compiling a closure), this is populated with its Locals.
    pub(super) parent: Option<Box<Self>>,

    scopes: Vec<Scope>,
    /// The next stack slot to be occupied by a variable.
    local_count: u32,
    /// The total amount of locals currently allocated. This is used to populate the
    /// `preallocate_stack_slots` field in chunks, to provide more efficient allocations
    allocated_local_count: u32,

    /// Variables captured from parent scopes.
    pub(super) captures: Vec<CaptureKind>,
}

impl Locals {
    /// Creates a new local.
    fn create_local(
        &mut self,
        name: &str,
        allocation: VariableAllocation,
    ) -> Result<VariablePlace, ErrorKind> {
        let slot = Opr24::new(self.local_count).map_err(|_| ErrorKind::TooManyLocals)?;
        let slot = LocalIndex(slot);
        let scope = self.scopes.last_mut().unwrap();
        scope
            .variables_by_name
            .insert(name.to_owned(), Variable { stack_slot: slot, is_captured: false });
        self.local_count += 1;
        if allocation == VariableAllocation::Allocate {
            self.allocated_local_count += 1;
            scope.allocated_variable_count += 1;
        }
        Ok(VariablePlace::Local(slot))
    }

    fn variables_in_scope_mut(&mut self) -> impl Iterator<Item = (&str, &'_ mut Variable)> {
        self.scopes
            .iter_mut()
            .rev()
            .flat_map(|scope| scope.variables_by_name.iter_mut().map(|(s, v)| (s.as_str(), v)))
    }

    /// Returns the index of the given capture.
    fn capture_index(&mut self, capture: CaptureKind) -> Result<UpvalueIndex, ErrorKind> {
        // Iterating over captures maybe isn't most efficient here but it's not like we have
        // thousands of them anyways. Unless somebody absolutely crazy starts writing Mica code.
        // Then all I can say is: I hate you.
        let index = self.captures.iter().rposition(|c| c.eq(&capture)).unwrap_or_else(|| {
            let index = self.captures.len();
            self.captures.push(capture);
            index
        });
        Ok(UpvalueIndex(Opr24::try_from(index).map_err(|_| ErrorKind::TooManyCaptures)?))
    }

    /// Performs a local variable lookup. This may modify parent Locals and capture upvalues.
    fn lookup(&mut self, name: &str) -> Result<Option<VariablePlace>, ErrorKind> {
        // Work inside out: try innermost scopes (own locals) first.
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables_by_name.get(name) {
                return Ok(Some(VariablePlace::Local(var.stack_slot)));
            }
        }
        // If there isn't a local with the given name, go up a level and look for locals to capture
        // or existing upvalues.
        if let Some(parent) = self.parent.as_mut() {
            if let Some(place) = parent.lookup(name)? {
                match place {
                    VariablePlace::Local(local_slot) => {
                        let (_, variable) = parent
                            .variables_in_scope_mut()
                            .find(|(_, var)| var.stack_slot == local_slot)
                            .unwrap();
                        variable.is_captured = true;
                        let stack_slot = variable.stack_slot;
                        let upvalue_index = self.capture_index(CaptureKind::Local(stack_slot))?;
                        return Ok(Some(VariablePlace::Upvalue(upvalue_index)));
                    }
                    VariablePlace::Upvalue(upvalue_index) => {
                        let own_index = self.capture_index(CaptureKind::Upvalue(upvalue_index))?;
                        return Ok(Some(VariablePlace::Upvalue(own_index)));
                    }
                    VariablePlace::Global(_) => unreachable!(),
                }
            }
        }
        Ok(None)
    }

    /// Pushes a new scope onto the scope stack.
    fn push_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    /// Pops the topmost scope off the scope stack and frees storage of any variables.
    fn pop_scope(&mut self) -> Scope {
        let scope = self.scopes.pop().expect("no scopes left on the stack");
        self.local_count -= scope.variables_by_name.len() as u32;
        self.allocated_local_count -= scope.allocated_variable_count;
        scope
    }
}

impl<'e> CodeGenerator<'e> {
    /// Creates a variable. If there is a scope on the stack, the variable is local; otherwise it
    /// is global.
    pub(super) fn create_variable(
        &mut self,
        name: &str,
        allocation: VariableAllocation,
    ) -> Result<VariablePlace, ErrorKind> {
        if !self.locals.scopes.is_empty() {
            let place = self.locals.create_local(name, allocation)?;
            self.chunk.preallocate_stack_slots =
                self.chunk.preallocate_stack_slots.max(self.locals.allocated_local_count);
            Ok(place)
        } else {
            let slot = self.env.create_global(name)?;
            Ok(VariablePlace::Global(slot))
        }
    }

    /// Performs a variable lookup. Returns the stack slot of the variable if it exists.
    /// Otherwise returns `None`.
    pub(super) fn lookup_variable(
        &mut self,
        name: &str,
    ) -> Result<Option<VariablePlace>, ErrorKind> {
        // Work from the inside out: check innermost local scopes first.
        if let Some(place) = self.locals.lookup(name)? {
            return Ok(Some(place));
        }
        // Lastly check globals.
        Ok(self.env.get_global(name).map(VariablePlace::Global))
    }

    /// Pushes a new scope onto the scope stack.
    pub(super) fn push_scope(&mut self) {
        self.locals.push_scope();
    }

    /// Pops the topmost scope off the scope stack and frees storage of any variables.
    pub(super) fn pop_scope(&mut self) {
        let scope = self.locals.pop_scope();
        for variable in scope.variables_by_name.into_values() {
            if variable.is_captured {
                self.chunk.emit((Opcode::CloseLocal, variable.stack_slot.0));
            }
        }
    }

    /// Generates a variable load instruction (GetLocal, GetGlobal, GetUpvalue).
    pub(super) fn generate_variable_load(&mut self, variable: VariablePlace) {
        self.chunk.emit(match variable {
            VariablePlace::Global(slot) => (Opcode::GetGlobal, slot.to_opr24()),
            VariablePlace::Local(slot) => (Opcode::GetLocal, slot.0),
            VariablePlace::Upvalue(slot) => (Opcode::GetUpvalue, slot.0),
        });
    }

    /// Generates a variable assign instruction (AssignLocal, AssignGlobal, AssignUpvalue).
    pub(super) fn generate_variable_assign(&mut self, variable: VariablePlace) {
        self.chunk.emit(match variable {
            VariablePlace::Global(slot) => (Opcode::AssignGlobal, slot.to_opr24()),
            VariablePlace::Local(slot) => (Opcode::AssignLocal, slot.0),
            VariablePlace::Upvalue(slot) => (Opcode::AssignUpvalue, slot.0),
        });
    }

    /// Generates a variable sink instruction (SinkLocal, SinkGlobal, SinkUpvalue).
    pub(super) fn generate_variable_sink(&mut self, variable: VariablePlace) {
        self.chunk.emit(match variable {
            VariablePlace::Global(slot) => (Opcode::SinkGlobal, slot.to_opr24()),
            VariablePlace::Local(slot) => (Opcode::SinkLocal, slot.0),
            VariablePlace::Upvalue(slot) => (Opcode::SinkUpvalue, slot.0),
        });
    }
}

impl<'e> CodeGenerator<'e> {
    /// Generates code for a variable lookup.
    pub(super) fn generate_variable(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        let name = ast.string(node).unwrap();
        if let Some(variable) = self.lookup_variable(name).map_err(|kind| ast.error(node, kind))? {
            self.generate_variable_load(variable);
            Ok(ExpressionResult::Present)
        } else {
            Err(ast.error(node, ErrorKind::VariableDoesNotExist(name.to_owned())))
        }
    }
}
