//! Bytecode generation.

use std::collections::{HashMap, HashSet};
use std::mem;
use std::rc::Rc;

use crate::ast::{Ast, NodeId, NodeKind};
use crate::bytecode::{
   CaptureKind, Chunk, Environment, Function, FunctionKind, FunctionSignature, Opcode, Opr24,
   Prototype,
};
use crate::common::{Error, ErrorKind, RenderedSignature};

/// Info about a local variable on the stack.
#[derive(Debug)]
struct Variable {
   stack_slot: Opr24,
   is_captured: bool,
}

#[derive(Debug, Default)]
struct Scope {
   /// Mapping from variable names to stack slots.
   variables_by_name: HashMap<String, Variable>,
   allocated_variable_count: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariablePlace {
   Global(Opr24),
   Local(Opr24),
   Upvalue(Opr24),
}

/// The kind of a variable allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableAllocation {
   /// Inherit the allocation from the caller (parameter passing).
   Inherit,
   /// Allocate new storage for the variable.
   Allocate,
}

/// Local variables, including upvalues.
#[derive(Default)]
struct Locals {
   /// If there is a parent code generator with its own scopes (the current instance is in the
   /// middle of compiling a closure), this is populated with its Locals.
   parent: Option<Box<Self>>,

   scopes: Vec<Scope>,
   /// The next stack slot to be occupied by a variable.
   local_count: u32,
   /// The total amount of locals currently allocated. This is used to populate the
   /// `preallocate_stack_slots` field in chunks, to provide more efficient allocations
   allocated_local_count: u32,

   /// Variables captured from parent scopes.
   captures: Vec<CaptureKind>,
}

impl Locals {
   /// Creates a new local.
   fn create_local(
      &mut self,
      name: &str,
      allocation: VariableAllocation,
   ) -> Result<VariablePlace, ErrorKind> {
      let slot = self.local_count;
      let slot = Opr24::new(slot).map_err(|_| ErrorKind::TooManyLocals)?;
      let scope = self.scopes.last_mut().unwrap();
      scope.variables_by_name.insert(
         name.to_owned(),
         Variable {
            stack_slot: slot,
            is_captured: false,
         },
      );
      self.local_count += 1;
      if allocation == VariableAllocation::Allocate {
         self.allocated_local_count += 1;
         scope.allocated_variable_count += 1;
      }
      Ok(VariablePlace::Local(slot))
   }

   fn variables_in_scope_mut(&mut self) -> impl Iterator<Item = (&str, &'_ mut Variable)> {
      self
         .scopes
         .iter_mut()
         .rev()
         .flat_map(|scope| scope.variables_by_name.iter_mut().map(|(s, v)| (s.as_str(), v)))
   }

   /// Returns the index of the given capture.
   fn capture_index(&mut self, capture: CaptureKind) -> Result<Opr24, ErrorKind> {
      // Iterating over captures maybe isn't most efficient here but it's not like we have
      // thousands of them anyways. Unless somebody absolutely crazy starts writing Mica code.
      // Then all I can say is: I hate you.
      let index = self.captures.iter().rposition(|c| c.eq(&capture)).unwrap_or_else(|| {
         let index = self.captures.len();
         self.captures.push(capture);
         index
      });
      Opr24::try_from(index).map_err(|_| ErrorKind::TooManyCaptures)
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

#[derive(Debug, Default)]
struct BreakableBlock {
   /// A list of offsets where `breaks` should be backpatched.
   breaks: Vec<usize>,
   start: usize,
}

#[derive(Debug, Default)]
struct StructData {
   /// Mapping from field names to indices.
   fields: HashMap<Rc<str>, Opr24>,
   /// The `self` variable. Used in `@field` lookups.
   receiver: Option<VariablePlace>,
}

impl StructData {
   /// Returns the index of the field with the given name, or creates that field if it doesn't
   /// exist yet.
   fn get_or_create_field(&mut self, name: &str) -> Result<Opr24, ErrorKind> {
      if !self.fields.contains_key(name) {
         let index = Opr24::try_from(self.fields.len()).map_err(|_| ErrorKind::TooManyFields)?;
         self.fields.insert(Rc::from(name), index);
         Ok(index)
      } else {
         Ok(*self.fields.get(name).unwrap())
      }
   }

   /// Returns the index of the field with the given name, or `None` if there is no such field.
   fn get_field(&self, name: &str) -> Option<Opr24> {
      self.fields.get(name).copied()
   }
}

pub struct CodeGenerator<'e> {
   env: &'e mut Environment,

   chunk: Chunk,

   locals: Box<Locals>,
   breakable_blocks: Vec<BreakableBlock>,
   struct_data: Option<Box<StructData>>,

   allow_new_fields: bool,
   is_constructor: bool,
   assigned_fields: HashSet<Rc<str>>,
}

impl<'e> CodeGenerator<'e> {
   /// Constructs a new code generator with an empty chunk.
   pub fn new(module_name: Rc<str>, env: &'e mut Environment) -> Self {
      Self {
         env,
         chunk: Chunk::new(module_name),

         locals: Default::default(),
         breakable_blocks: Vec::new(),
         struct_data: None,

         allow_new_fields: false,
         is_constructor: false,
         assigned_fields: HashSet::new(),
      }
   }

   /// Creates a variable. If there is a scope on the stack, the variable is local; otherwise it
   /// is global.
   fn create_variable(
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
   fn lookup_variable(&mut self, name: &str) -> Result<Option<VariablePlace>, ErrorKind> {
      // Work from the inside out: check innermost local scopes first.
      if let Some(place) = self.locals.lookup(name)? {
         return Ok(Some(place));
      }
      // Lastly check globals.
      Ok(self.env.get_global(name).map(VariablePlace::Global))
   }

   /// Pushes a new scope onto the scope stack.
   fn push_scope(&mut self) {
      self.locals.push_scope();
   }

   /// Pops the topmost scope off the scope stack and frees storage of any variables.
   fn pop_scope(&mut self) {
      let scope = self.locals.pop_scope();
      for variable in scope.variables_by_name.into_values() {
         if variable.is_captured {
            self.chunk.emit((Opcode::CloseLocal, variable.stack_slot));
         }
      }
   }

   /// Generates a variable load instruction (GetLocal, GetGlobal, GetUpvalue).
   fn generate_variable_load(&mut self, variable: VariablePlace) {
      self.chunk.emit(match variable {
         VariablePlace::Global(slot) => (Opcode::GetGlobal, slot),
         VariablePlace::Local(slot) => (Opcode::GetLocal, slot),
         VariablePlace::Upvalue(slot) => (Opcode::GetUpvalue, slot),
      });
   }

   /// Generates a variable assign instruction (AssignLocal, AssignGlobal, AssignUpvalue).
   fn generate_variable_assign(&mut self, variable: VariablePlace) {
      self.chunk.emit(match variable {
         VariablePlace::Global(slot) => (Opcode::AssignGlobal, slot),
         VariablePlace::Local(slot) => (Opcode::AssignLocal, slot),
         VariablePlace::Upvalue(slot) => (Opcode::AssignUpvalue, slot),
      });
   }

   /// Generates a variable sink instruction (SinkLocal, SinkGlobal, SinkUpvalue).
   fn generate_variable_sink(&mut self, variable: VariablePlace) {
      self.chunk.emit(match variable {
         VariablePlace::Global(slot) => (Opcode::SinkGlobal, slot),
         VariablePlace::Local(slot) => (Opcode::SinkLocal, slot),
         VariablePlace::Upvalue(slot) => (Opcode::SinkUpvalue, slot),
      });
   }

   /// Pushes a new breakable block.
   fn push_breakable_block(&mut self) {
      let start = self.chunk.emit(Opcode::Nop);
      self.breakable_blocks.push(BreakableBlock {
         breaks: Vec::new(),
         start,
      });
   }

   /// Pops the topmost breakable block.
   fn pop_breakable_block(&mut self) {
      let block = self.breakable_blocks.pop().unwrap();
      if !block.breaks.is_empty() {
         self.chunk.patch(block.start, Opcode::EnterBreakableBlock);
         for jump in block.breaks {
            // Unwrapping is safe here because if the loop is too large the error was caught already
            // before `pop_breakable_block` was called.
            self.chunk.patch(jump, Opcode::jump_forward(jump, self.chunk.len()).unwrap());
         }
         self.chunk.emit((Opcode::ExitBreakableBlock, 1));
      }
   }

   /// Generates code for a list of nodes. The last node's value is the one left on the stack.
   ///
   /// If there are no nodes in the list, this is equivalent to a `nil` literal.
   fn generate_node_list(&mut self, ast: &Ast, nodes: &[NodeId]) -> Result<(), Error> {
      if nodes.is_empty() {
         let _ = self.generate_nil();
      } else {
         for (i, &node) in nodes.iter().enumerate() {
            self.generate_node(
               ast,
               node,
               if i != nodes.len() - 1 {
                  Expression::Discarded
               } else {
                  Expression::Used
               },
            )?;
         }
      }
      Ok(())
   }

   /// Generates code for a nil literal.
   fn generate_nil(&mut self) -> ExpressionResult {
      self.chunk.emit(Opcode::PushNil);
      ExpressionResult::Present
   }

   /// Generates code for a boolean literal.
   fn generate_boolean(&mut self, ast: &Ast, node: NodeId) -> ExpressionResult {
      self.chunk.emit(match ast.kind(node) {
         NodeKind::True => Opcode::PushTrue,
         NodeKind::False => Opcode::PushFalse,
         _ => unreachable!(),
      });
      ExpressionResult::Present
   }

   /// Generates code for a number literal.
   fn generate_number(&mut self, ast: &Ast, node: NodeId) -> ExpressionResult {
      self.chunk.emit(Opcode::PushNumber);
      let number = ast.number(node).unwrap();
      self.chunk.emit_number(number);
      ExpressionResult::Present
   }

   /// Generates code for a string literal.
   fn generate_string(&mut self, ast: &Ast, node: NodeId) -> ExpressionResult {
      self.chunk.emit(Opcode::PushString);
      let string = ast.string(node).unwrap();
      self.chunk.emit_string(string);
      ExpressionResult::Present
   }

   /// Generates code for a unary operator.
   fn generate_unary(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (left, _) = ast.node_pair(node);
      self.generate_node(ast, left, Expression::Used)?;
      match ast.kind(node) {
         NodeKind::Negate => self.chunk.emit(Opcode::Negate),
         NodeKind::Not => self.chunk.emit(Opcode::Not),
         _ => unreachable!(),
      };
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a binary operator.
   fn generate_binary(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (left, right) = ast.node_pair(node);
      self.generate_node(ast, left, Expression::Used)?;
      self.generate_node(ast, right, Expression::Used)?;
      match ast.kind(node) {
         NodeKind::Negate => self.chunk.emit(Opcode::Negate),

         NodeKind::Add => self.chunk.emit(Opcode::Add),
         NodeKind::Subtract => self.chunk.emit(Opcode::Subtract),
         NodeKind::Multiply => self.chunk.emit(Opcode::Multiply),
         NodeKind::Divide => self.chunk.emit(Opcode::Divide),

         NodeKind::Equal => self.chunk.emit(Opcode::Equal),
         NodeKind::NotEqual => {
            self.chunk.emit(Opcode::Equal);
            self.chunk.emit(Opcode::Not)
         }
         NodeKind::Less => self.chunk.emit(Opcode::Less),
         NodeKind::LessEqual => self.chunk.emit(Opcode::LessEqual),
         NodeKind::Greater => {
            self.chunk.emit(Opcode::Swap);
            self.chunk.emit(Opcode::Less)
         }
         NodeKind::GreaterEqual => {
            self.chunk.emit(Opcode::Swap);
            self.chunk.emit(Opcode::LessEqual)
         }
         _ => unreachable!(),
      };
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a variable lookup.
   fn generate_variable(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let name = ast.string(node).unwrap();
      if let Some(variable) = self.lookup_variable(name).map_err(|kind| ast.error(node, kind))? {
         self.generate_variable_load(variable);
         Ok(ExpressionResult::Present)
      } else {
         Err(ast.error(node, ErrorKind::VariableDoesNotExist(name.to_owned())))
      }
   }

   /// Generates code for a field lookup.
   fn generate_field(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (name, _) = ast.node_pair(node);
      let name = ast.string(name).unwrap();
      let struct_data = self
         .struct_data
         .as_deref()
         .ok_or_else(|| ast.error(node, ErrorKind::FieldOutsideOfImpl))?;
      let field_id = struct_data
         .get_field(name)
         .ok_or_else(|| ast.error(node, ErrorKind::FieldDoesNotExist(Rc::clone(name))))?;
      // Unwrapping is OK here because fields are not allowed outside of functions, and each
      // function with `StructData` passed in assigns to `receiver` at the start of its
      // code generation.
      let receiver = struct_data.receiver.unwrap();
      self.generate_variable_load(receiver);
      self.chunk.emit((Opcode::GetField, field_id));
      Ok(ExpressionResult::Present)
   }

   /// Generates code for an assignment.
   fn generate_assignment(
      &mut self,
      ast: &Ast,
      node: NodeId,
      result: Expression,
   ) -> Result<ExpressionResult, Error> {
      let (target, value) = ast.node_pair(node);
      self.generate_node(ast, value, Expression::Used)?;

      match ast.kind(target) {
         NodeKind::Identifier => {
            let name = ast.string(target).unwrap();
            let variable = if let Some(slot) =
               self.lookup_variable(name).map_err(|kind| ast.error(target, kind))?
            {
               slot
            } else {
               self
                  .create_variable(name, VariableAllocation::Allocate)
                  .map_err(|kind| ast.error(node, kind))?
            };
            match result {
               Expression::Used => self.generate_variable_assign(variable),
               Expression::Discarded => self.generate_variable_sink(variable),
            }
         }
         NodeKind::Field => {
            let (name, _) = ast.node_pair(target);
            let name = ast.string(name).unwrap();
            if let Some(struct_data) = self.struct_data.as_mut() {
               let field = if self.allow_new_fields {
                  struct_data.get_or_create_field(name).map_err(|kind| ast.error(node, kind))?
               } else {
                  struct_data.get_field(name).ok_or_else(|| {
                     ast.error(target, ErrorKind::FieldDoesNotExist(Rc::clone(name)))
                  })?
               };
               // Unwrapping is OK here because `receiver` is assigned at the start of each function
               // in an `impl` block.
               let receiver = struct_data.receiver.unwrap();
               self.generate_variable_load(receiver);
               self.chunk.emit(match result {
                  Expression::Used => (Opcode::AssignField, field),
                  Expression::Discarded => (Opcode::SinkField, field),
               });
            } else {
               return Err(ast.error(target, ErrorKind::FieldOutsideOfImpl));
            }
            // In constructors, we need to keep track of which fields were assigned to report
            // errors about missing field values.
            if self.is_constructor && !self.allow_new_fields {
               self.assigned_fields.insert(Rc::clone(name));
            }
         }
         _ => return Err(ast.error(target, ErrorKind::InvalidAssignment)),
      }

      Ok(match result {
         Expression::Discarded => ExpressionResult::Absent,
         Expression::Used => ExpressionResult::Present,
      })
   }

   /// Generates code for a list literal.
   fn generate_list(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let children = ast.children(node).unwrap();
      let len =
         Opr24::try_from(children.len()).map_err(|_| ast.error(node, ErrorKind::ListIsTooLong))?;
      for &child in children {
         self.generate_node(ast, child, Expression::Used)?;
      }
      self.chunk.emit((Opcode::CreateList, len));
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a dict literal.
   fn generate_dict(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let children = ast.children(node).unwrap();
      let len =
         Opr24::try_from(children.len()).map_err(|_| ast.error(node, ErrorKind::DictIsTooLarge))?;
      for &child in children {
         let (key, value) = ast.node_pair(child);
         self.generate_node(ast, key, Expression::Used)?;
         self.generate_node(ast, value, Expression::Used)?;
      }
      self.chunk.emit((Opcode::CreateDict, len));
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a `do..end` expression.
   fn generate_do(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let children = ast.children(node).unwrap();
      self.push_scope();
      self.generate_node_list(ast, children)?;
      self.pop_scope();
      Ok(ExpressionResult::Present)
   }

   /// Generates code for an `if..elif..else..end` expression.
   fn generate_if(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let branches = ast.children(node).unwrap();
      let mut jumps_to_end = Vec::new();

      for (i, &branch) in branches.iter().enumerate() {
         // We need to discard the previous branch's condition (if there was a previous branch).
         if i > 0 {
            self.chunk.emit(Opcode::Discard);
         }

         let then = ast.children(branch).unwrap();
         match ast.kind(branch) {
            NodeKind::IfBranch => {
               // Generate the condition.
               let (condition, _) = ast.node_pair(branch);
               self.push_scope();
               self.generate_node(ast, condition, Expression::Used)?;
               // Generate a Nop that is later backpatched with a ConditionalJumpForward.
               let jump = self.chunk.emit(Opcode::Nop);
               self.chunk.emit(Opcode::Discard); // The condition has to be discarded.
               self.generate_node_list(ast, then)?;
               self.pop_scope();
               let jump_to_end = self.chunk.emit(Opcode::Nop);
               jumps_to_end.push(jump_to_end);
               self.chunk.patch(
                  jump,
                  Opcode::jump_forward_if_falsy(jump, self.chunk.len())
                     .map_err(|_| ast.error(branch, ErrorKind::IfBranchTooLarge))?,
               );
            }

            NodeKind::ElseBranch => {
               self.push_scope();
               self.generate_node_list(ast, then)?;
               self.pop_scope();
            }

            _ => unreachable!(),
         }
      }

      // If there was no `else` branch, we need to patch in an implicit one that returns `nil`.
      if ast.kind(*branches.last().unwrap()) != NodeKind::ElseBranch {
         self.chunk.emit(Opcode::Discard);
         self.chunk.emit(Opcode::PushNil);
      }

      // Backpatch all jumps to end with an unconditional jump forward.
      for jump in jumps_to_end {
         self.chunk.patch(
            jump,
            Opcode::jump_forward(jump, self.chunk.len())
               .map_err(|_| ast.error(node, ErrorKind::IfExpressionTooLarge))?,
         );
      }

      Ok(ExpressionResult::Present)
   }

   /// Generates code for an `and` infix operator.
   fn generate_and(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (left, right) = ast.node_pair(node);
      self.push_scope();
      self.generate_node(ast, left, Expression::Used)?;
      let jump_past_right = self.chunk.emit(Opcode::Nop);
      self.chunk.emit(Opcode::Discard);
      self.generate_node(ast, right, Expression::Used)?;
      self.chunk.patch(
         jump_past_right,
         Opcode::jump_forward_if_falsy(jump_past_right, self.chunk.len())
            .map_err(|_| ast.error(node, ErrorKind::OperatorRhsTooLarge))?,
      );
      self.pop_scope();
      Ok(ExpressionResult::Present)
   }

   /// Generates code for an `or` infix operator.
   fn generate_or(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (left, right) = ast.node_pair(node);
      self.push_scope();
      self.generate_node(ast, left, Expression::Used)?;
      let jump_past_right = self.chunk.emit(Opcode::Nop);
      self.chunk.emit(Opcode::Discard);
      self.generate_node(ast, right, Expression::Used)?;
      self.chunk.patch(
         jump_past_right,
         Opcode::jump_forward_if_truthy(jump_past_right, self.chunk.len())
            .map_err(|_| ast.error(node, ErrorKind::OperatorRhsTooLarge))?,
      );
      self.pop_scope();
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a `while..do..end` loop.
   fn generate_while(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (condition, _) = ast.node_pair(node);
      let body = ast.children(node).unwrap();

      // The outer scope, so that variables can be declared in the condition.
      self.push_scope();
      // The breakable block.
      self.push_breakable_block();

      let start = self.chunk.len();
      self.generate_node(ast, condition, Expression::Used)?;
      let jump_to_end = self.chunk.emit(Opcode::Nop);
      // Discard the condition if it's true.
      self.chunk.emit(Opcode::Discard);

      self.generate_node_list(ast, body)?;
      // While loops don't yield a value.
      self.chunk.emit(Opcode::Discard);

      self.chunk.emit(
         Opcode::jump_backward(self.chunk.len(), start)
            .map_err(|_| ast.error(node, ErrorKind::LoopTooLarge))?,
      );
      self.chunk.patch(
         jump_to_end,
         Opcode::jump_forward_if_falsy(jump_to_end, self.chunk.len())
            .map_err(|_| ast.error(node, ErrorKind::LoopTooLarge))?,
      );
      // Discard the condition if it's false.
      self.chunk.emit(Opcode::Discard);

      // Because while loops are an expression, they must produce a value. That value is `nil`.
      self.chunk.emit(Opcode::PushNil);

      // `break`s produce a value (or `nil` by default), so we need to jump over the
      // fallback `PushNil`.
      self.pop_breakable_block();
      self.pop_scope();

      Ok(ExpressionResult::Present)
   }

   /// Generates a `break` expression.
   fn generate_break(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (right, _) = ast.node_pair(node);
      if right != NodeId::EMPTY {
         self.generate_node(ast, right, Expression::Used)?;
      } else {
         let _ = self.generate_nil();
      }
      let jump = self.chunk.emit(Opcode::Nop);
      if let Some(block) = self.breakable_blocks.last_mut() {
         block.breaks.push(jump);
      } else {
         return Err(ast.error(node, ErrorKind::BreakOutsideOfLoop));
      }
      Ok(ExpressionResult::NoReturn)
   }

   /// Generates code for a function call.
   fn generate_call(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (function, _) = ast.node_pair(node);
      match ast.kind(function) {
         // Method calls need special treatment.
         NodeKind::Dot => {
            let (receiver, name) = ast.node_pair(function);
            if ast.kind(name) != NodeKind::Identifier {
               return Err(ast.error(name, ErrorKind::InvalidMethodName));
            }
            let name = ast.string(name).unwrap();

            // Generate the receiver and arguments.
            self.generate_node(ast, receiver, Expression::Used)?;
            let arguments = ast.children(node).unwrap();
            for &argument in arguments {
               self.generate_node(ast, argument, Expression::Used)?;
            }

            // Construct the call.
            let arity: u8 = (1 + arguments.len())
               // ↑ Add 1 for the receiver, which is also an argument.
               .try_into()
               .map_err(|_| ast.error(node, ErrorKind::TooManyArguments))?;
            let signature = FunctionSignature::new(Rc::clone(name), arity as u16);
            let method_index =
               self.env.get_method_index(&signature).map_err(|kind| ast.error(node, kind))?;
            self.chunk.emit((Opcode::CallMethod, Opr24::pack((method_index, arity))));
         }
         _ => {
            self.generate_node(ast, function, Expression::Used)?;
            let arguments = ast.children(node).unwrap();
            for &argument in arguments {
               self.generate_node(ast, argument, Expression::Used)?;
            }
            self.chunk.emit((
               Opcode::Call,
               Opr24::try_from(arguments.len())
                  .map_err(|_| ast.error(node, ErrorKind::TooManyArguments))?,
            ));
         }
      }
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a bare dot call (without parentheses).
   fn generate_dot(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (left, method) = ast.node_pair(node);
      self.generate_node(ast, left, Expression::Used)?;

      if ast.kind(method) != NodeKind::Identifier {
         return Err(ast.error(method, ErrorKind::InvalidMethodName));
      }
      let signature = FunctionSignature::new(Rc::clone(ast.string(method).unwrap()), 1);
      let method_index =
         self.env.get_method_index(&signature).map_err(|kind| ast.error(node, kind))?;
      self.chunk.emit((Opcode::CallMethod, Opr24::pack((method_index, 1))));

      Ok(ExpressionResult::Present)
   }

   fn generate_function(
      &mut self,
      ast: &Ast,
      node: NodeId,
      GenerateFunctionOptions { name, call_conv }: GenerateFunctionOptions,
   ) -> Result<GeneratedFunction, Error> {
      let (head, body) = ast.node_pair(node);
      let (_, parameters) = ast.node_pair(head);
      let parameter_list = ast.children(parameters).unwrap();

      let mut generator = CodeGenerator::new(Rc::clone(&self.chunk.module_name), self.env);
      // NOTE: Hopefully the allocation from this mem::take gets optimized out.
      generator.locals.parent = Some(mem::take(&mut self.locals));
      if call_conv.has_field_access() {
         generator.struct_data = self.struct_data.take();
         generator.allow_new_fields = call_conv.allow_new_fields();
      }
      generator.is_constructor = call_conv.is_constructor();

      // Push a scope to start creating local variables.
      generator.push_scope();
      // Create local variables for all the parameters.
      // Note that in bare functions, the function itself acts as the `self` parameter.
      let receiver = if call_conv.has_visible_self() {
         let receiver = generator.create_variable("self", VariableAllocation::Inherit).unwrap();
         if let Some(struct_data) = generator.struct_data.as_deref_mut() {
            struct_data.receiver = Some(receiver);
         }
         receiver
      } else {
         generator.create_variable("<receiver>", VariableAllocation::Inherit).unwrap()
      };
      for &parameter in parameter_list {
         let parameter_name = ast.string(parameter).unwrap();
         generator
            .create_variable(parameter_name, VariableAllocation::Inherit)
            .map_err(|kind| ast.error(parameter, kind))?;
      }

      // In constructors, we have to create `self` explicitly.
      let create_struct = if call_conv.is_constructor() {
         generator.generate_variable_load(receiver);
         let instruction = generator.chunk.emit(Opcode::Nop);
         let receiver = generator
            .create_variable("self", VariableAllocation::Allocate)
            .map_err(|kind| ast.error(node, kind))?;
         generator.generate_variable_assign(receiver);
         generator.chunk.emit(Opcode::Discard);
         generator.struct_data.as_deref_mut().unwrap().receiver = Some(receiver);
         Some(instruction)
      } else {
         None
      };

      // Generate the body.
      generator.generate_node(ast, body, Expression::Used)?;

      // If we're in a constructor we have to backpatch the `CreateStruct` instruction with the
      // number of fields.
      if let Some(create_struct) = create_struct {
         let field_count = generator.struct_data.as_ref().unwrap().fields.len();
         let field_count = Opr24::try_from(field_count)
            .map_err(|_| ast.error(ast.node_pair(parameters).0, ErrorKind::TooManyFields))?;
         generator.chunk.patch(create_struct, (Opcode::CreateStruct, field_count));
         // We also have to discard whatever was at the top of the stack at the moment and
         // return the struct we constructed.
         generator.chunk.emit(Opcode::Discard);
         let receiver = generator.struct_data.as_ref().unwrap().receiver.unwrap();
         generator.generate_variable_load(receiver);
      }
      // If we're in a constructor, we also have to check if all fields were assigned.
      if call_conv.is_constructor() && !call_conv.allow_new_fields() {
         let struct_data = generator.struct_data.as_ref().unwrap();
         let missing: Vec<_> = struct_data
            .fields
            .keys()
            .filter(|field| !generator.assigned_fields.contains(*field))
            .cloned()
            .collect();
         if !missing.is_empty() {
            return Err(ast.error(node, ErrorKind::MissingFields(missing)));
         }
      }

      // Finish generating the chunk by inserting a `Return` opcode.
      generator.pop_scope();
      generator.chunk.emit(Opcode::Return);

      // Take back what was taken from the parent generator.
      self.locals = generator.locals.parent.take().unwrap();
      if call_conv.has_field_access() {
         self.struct_data = generator.struct_data;
      }

      // Construct the function.
      let parameter_count = u16::try_from(parameter_list.len())
         .map_err(|_| ast.error(parameters, ErrorKind::TooManyParameters))?;
      let function = Function {
         name,
         parameter_count: Some(parameter_count),
         kind: FunctionKind::Bytecode {
            chunk: Rc::new(generator.chunk),
            captured_locals: generator.locals.captures,
         },
         hidden_in_stack_traces: false,
      };
      let function_id = self.env.create_function(function).map_err(|kind| ast.error(node, kind))?;

      Ok(GeneratedFunction {
         id: function_id,
         parameter_count,
      })
   }

   /// Ensures that a `Func` node is a valid bare function - without a kind, and with a body.
   fn ensure_valid_bare_function(ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (head, body) = ast.node_pair(node);
      let (_name, parameters) = ast.node_pair(head);

      let function_kind = ast.node_pair(parameters).0;
      if function_kind != NodeId::EMPTY {
         return Err(ast.error(function_kind, ErrorKind::FunctionKindOutsideImpl));
      }
      if body == NodeId::EMPTY {
         return Err(ast.error(node, ErrorKind::MissingFunctionBody));
      }
      Ok(())
   }

   /// Generates code for a bare function declaration.
   fn generate_function_declaration(
      &mut self,
      ast: &Ast,
      node: NodeId,
   ) -> Result<ExpressionResult, Error> {
      Self::ensure_valid_bare_function(ast, node)?;

      let (head, _) = ast.node_pair(node);
      let (name_node, _) = ast.node_pair(head);
      let name = ast.string(name_node).unwrap();

      // Create the variable before compiling the function, to allow for recursion.
      let variable = self
         .create_variable(name, VariableAllocation::Allocate)
         .map_err(|kind| ast.error(name_node, kind))?;

      let function = self.generate_function(
         ast,
         node,
         GenerateFunctionOptions {
            name: Rc::clone(name),
            call_conv: FunctionCallConv::Bare,
         },
      )?;

      self.chunk.emit((Opcode::CreateClosure, function.id));
      self.generate_variable_sink(variable);

      Ok(ExpressionResult::Absent)
   }

   /// Generates code for a function expression.
   fn generate_function_expression(
      &mut self,
      ast: &Ast,
      node: NodeId,
   ) -> Result<ExpressionResult, Error> {
      Self::ensure_valid_bare_function(ast, node)?;

      let function = self.generate_function(
         ast,
         node,
         GenerateFunctionOptions {
            name: Rc::from("<anonymous>"),
            call_conv: FunctionCallConv::Bare,
         },
      )?;

      self.chunk.emit((Opcode::CreateClosure, function.id));
      Ok(ExpressionResult::Present)
   }

   /// Generates code for a `return` expression.
   fn generate_return(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (value, _) = ast.node_pair(node);
      // Unlike breaking out of a loop, returning is super simple, as it implies that all locals
      // are taken off the stack.
      if value == NodeId::EMPTY {
         let _ = self.generate_nil();
      } else {
         self.generate_node(ast, value, Expression::Used)?;
      }
      self.chunk.emit(Opcode::Return);
      Ok(ExpressionResult::NoReturn)
   }

   /// Generates code for a struct declaration.
   fn generate_struct(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (name, _) = ast.node_pair(node);
      let name = ast.string(name).unwrap();

      self.chunk.emit(Opcode::CreateType);
      self.chunk.emit_string(name);
      let variable = self
         .create_variable(name, VariableAllocation::Allocate)
         .map_err(|kind| ast.error(node, kind))?;
      self.generate_variable_assign(variable);

      Ok(ExpressionResult::Present)
   }

   /// Generates code for an `impl` block.
   fn generate_impl(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (implementee, _) = ast.node_pair(node);
      let items = ast.children(node).unwrap();

      let mut proto = Prototype::default();
      // There's no need to save any old struct data because `impl` blocks don't nest freely.
      // Yes, you can create an `impl` block in a function inside this `impl` block, but that
      // function is handled by a different generator.
      self.struct_data = Some(Box::new(StructData::default()));

      let mut has_constructor = false;
      for &item in items {
         match ast.kind(item) {
            NodeKind::Func => {
               let (head, _) = ast.node_pair(item);
               let (name_node, parameters) = ast.node_pair(head);
               if ast.kind(name_node) != NodeKind::Identifier {
                  return Err(ast.error(item, ErrorKind::MissingMethodName));
               }
               let name = Rc::clone(ast.string(name_node).unwrap());
               let (kind, _) = ast.node_pair(parameters);
               let function = self.generate_function(
                  ast,
                  item,
                  GenerateFunctionOptions {
                     name: Rc::clone(&name),
                     call_conv: match ast.kind(kind) {
                        NodeKind::Empty => FunctionCallConv::Instance,
                        NodeKind::Static => FunctionCallConv::Static,
                        NodeKind::Constructor => {
                           let allow_new_fields = !has_constructor;
                           has_constructor = true;
                           FunctionCallConv::Constructor { allow_new_fields }
                        }
                        _ => unreachable!(),
                     },
                  },
               )?;
               let map = match ast.kind(kind) {
                  NodeKind::Empty => &mut proto.instance,
                  NodeKind::Static | NodeKind::Constructor => &mut proto.statics,
                  _ => unreachable!(),
               };
               let signature = FunctionSignature::new(name, 1 + function.parameter_count);
               let method_id =
                  self.env.get_method_index(&signature).map_err(|kind| ast.error(item, kind))?;
               if map.contains_key(&method_id) {
                  return Err(ast.error(
                     name_node,
                     ErrorKind::MethodAlreadyImplemented(RenderedSignature {
                        name: signature.name,
                        arity: signature.arity.map(|x| x - 1),
                        trait_name: None,
                     }),
                  ));
               }
               map.insert(method_id, function.id);
            }
            // NB: If other item types are allowed, don't forget to change the error message for
            // InvalidImplItem.
            _ => return Err(ast.error(item, ErrorKind::InvalidImplItem)),
         }
      }

      let proto_id = self.env.create_prototype(proto).map_err(|kind| ast.error(node, kind))?;
      self.generate_node(ast, implementee, Expression::Used)?;
      self.chunk.emit((Opcode::Implement, proto_id));

      self.struct_data = None;

      Ok(ExpressionResult::Present)
   }

   /// Generates the *shim* for a trait method. The shim is responsible for calling the actual
   /// method.
   fn generate_trait_method_shim(
      &mut self,
      ast: &Ast,
      node: NodeId,
      trait_name: &str,
      method_id: u16,
      method_signature: &FunctionSignature,
   ) -> Result<Opr24, Error> {
      // NOTE: Overflow here should never happen because we're referring to
      let arity = method_signature.arity.unwrap() as u8;

      let mut chunk = Chunk::new(Rc::clone(&self.chunk.module_name));
      chunk.codegen_location = self.chunk.codegen_location;
      for arg in 1..arity {
         chunk.emit((Opcode::GetLocal, Opr24::from(arg)));
      }
      chunk.emit((Opcode::CallMethod, Opr24::pack((method_id, arity as u8))));

      let shim_name = Rc::from(format!("trait {trait_name}.{}", method_signature.name));
      let chunk = Rc::new(chunk);
      let function_id = self
         .env
         .create_function(Function {
            name: shim_name,
            parameter_count: method_signature.arity,
            kind: FunctionKind::Bytecode {
               chunk,
               captured_locals: vec![],
            },
            hidden_in_stack_traces: true,
         })
         .map_err(|e| ast.error(node, e))?;

      Ok(function_id)
   }

   fn generate_trait(&mut self, ast: &Ast, node: NodeId) -> Result<ExpressionResult, Error> {
      let (trait_name, _) = ast.node_pair(node);
      let trait_name = ast.string(trait_name).unwrap();
      let items = ast.children(node).unwrap();

      let trait_index =
         self.env.create_trait(Rc::clone(trait_name)).map_err(|k| ast.error(node, k))?;
      let mut required_methods = HashSet::new();
      let mut shims = Vec::new();

      for &item in items {
         match ast.kind(item) {
            NodeKind::Func => {
               let (head, body) = ast.node_pair(item);
               let (name, params) = ast.node_pair(head);
               if body != NodeId::EMPTY {
                  return Err(ast.error(body, ErrorKind::TraitMethodCannotHaveBody));
               }
               if name == NodeId::EMPTY {
                  return Err(ast.error(head, ErrorKind::MissingMethodName));
               }

               let signature = FunctionSignature {
                  name: Rc::clone(ast.string(name).unwrap()),
                  arity: Some(
                     1 + u16::try_from(ast.len(params).unwrap())
                        .map_err(|_| ast.error(params, ErrorKind::TooManyParameters))?,
                  ),
                  trait_index: Some(trait_index),
               };
               let method_id =
                  self.env.get_method_index(&signature).map_err(|e| ast.error(item, e))?;

               let shim_signature = FunctionSignature {
                  // Need to add 1 to the arity for the receiving trait.
                  arity: signature.arity.map(|x| x + 1),
                  trait_index: None,
                  ..signature.clone()
               };
               let shim_method_id =
                  self.env.get_method_index(&shim_signature).map_err(|e| ast.error(item, e))?;
               let shim_function_id =
                  self.generate_trait_method_shim(ast, item, trait_name, method_id, &signature)?;
               shims.push((shim_method_id, shim_function_id));

               if !required_methods.insert(method_id) {
                  return Err(ast.error(
                     item,
                     ErrorKind::TraitAlreadyHasMethod(RenderedSignature {
                        name: signature.name,
                        arity: signature.arity,
                        // Do not duplicate the trait name in the error message.
                        trait_name: None,
                     }),
                  ));
               }
            }
            _ => return Err(ast.error(item, ErrorKind::InvalidTraitItem)),
         }
      }

      let prototype = self.env.get_trait_mut(trait_index).unwrap();
      prototype.required = required_methods.into_iter().collect();
      prototype.shims = shims;

      self.chunk.emit((Opcode::CreateTrait, trait_index));
      let variable = self
         .create_variable(trait_name, VariableAllocation::Allocate)
         .map_err(|k| ast.error(node, k))?;
      self.generate_variable_assign(variable);

      Ok(ExpressionResult::Present)
   }

   /// Generates code for a single node.
   fn generate_node(&mut self, ast: &Ast, node: NodeId, expr: Expression) -> Result<(), Error> {
      let previous_codegen_location = self.chunk.codegen_location;
      self.chunk.codegen_location = ast.location(node);
      let result = match ast.kind(node) {
         NodeKind::Empty => panic!("empty nodes must never be generated"),

         NodeKind::Nil => self.generate_nil(),
         NodeKind::False | NodeKind::True => self.generate_boolean(ast, node),
         NodeKind::Number => self.generate_number(ast, node),
         NodeKind::String => self.generate_string(ast, node),

         NodeKind::Identifier => self.generate_variable(ast, node)?,

         NodeKind::List => self.generate_list(ast, node)?,
         NodeKind::Dict => self.generate_dict(ast, node)?,

         NodeKind::Negate | NodeKind::Not => self.generate_unary(ast, node)?,

         | NodeKind::Add
         | NodeKind::Subtract
         | NodeKind::Multiply
         | NodeKind::Divide
         | NodeKind::Equal
         | NodeKind::NotEqual
         | NodeKind::Less
         | NodeKind::Greater
         | NodeKind::LessEqual
         | NodeKind::GreaterEqual => self.generate_binary(ast, node)?,

         NodeKind::And => self.generate_and(ast, node)?,
         NodeKind::Or => self.generate_or(ast, node)?,

         NodeKind::Assign => self.generate_assignment(ast, node, expr)?,
         NodeKind::Dot => self.generate_dot(ast, node)?,
         NodeKind::Field => self.generate_field(ast, node)?,

         NodeKind::Main => {
            self.generate_node_list(ast, ast.children(node).unwrap())?;
            ExpressionResult::Present
         }

         NodeKind::Do => self.generate_do(ast, node)?,
         NodeKind::If => self.generate_if(ast, node)?,
         NodeKind::While => self.generate_while(ast, node)?,
         NodeKind::Break => self.generate_break(ast, node)?,

         NodeKind::Func => {
            let (head, _) = ast.node_pair(node);
            let (name, _) = ast.node_pair(head);
            if name != NodeId::EMPTY {
               self.generate_function_declaration(ast, node)?
            } else {
               self.generate_function_expression(ast, node)?
            }
         }
         NodeKind::Call => self.generate_call(ast, node)?,
         NodeKind::Return => self.generate_return(ast, node)?,

         NodeKind::Struct => self.generate_struct(ast, node)?,
         NodeKind::Impl => self.generate_impl(ast, node)?,
         NodeKind::Trait => self.generate_trait(ast, node)?,

         | NodeKind::DictPair
         | NodeKind::IfBranch
         | NodeKind::ElseBranch
         | NodeKind::FunctionHead
         | NodeKind::Parameters
         | NodeKind::Static
         | NodeKind::Constructor => {
            unreachable!("AST implementation detail")
         }
      };
      match (result, expr) {
         (ExpressionResult::Absent, Expression::Used) => {
            let _ = self.generate_nil();
         }
         (ExpressionResult::Present, Expression::Discarded) => {
            let _ = self.chunk.emit(Opcode::Discard);
         }
         _ => (),
      }
      self.chunk.codegen_location = previous_codegen_location;
      Ok(())
   }

   /// Generates code for the given AST.
   pub fn generate(mut self, ast: &Ast, root_node: NodeId) -> Result<Rc<Chunk>, Error> {
      self.generate_node(ast, root_node, Expression::Used)?;
      self.chunk.emit(Opcode::Halt);
      Ok(Rc::new(self.chunk))
   }
}

/// The calling convention of a function.
enum FunctionCallConv {
   /// A bare function: `self` is invisible, fields cannot be assigned.
   Bare,
   /// A static function: `self` is visible, fields cannot be assigned.
   Static,
   /// An instance function: `self` is visible, fields can be assigned but not created.
   Instance,
   /// A constructor: `self` is created from scratch, fields can created if `allow_new_fields` is
   /// true.
   Constructor { allow_new_fields: bool },
}

impl FunctionCallConv {
   fn has_field_access(&self) -> bool {
      matches!(self, Self::Instance | Self::Constructor { .. })
   }

   fn has_visible_self(&self) -> bool {
      // Note that `Constructor` does not have a visible `self`, because it's an allocated local
      // rather than inherited.
      matches!(self, Self::Static | Self::Instance)
   }

   fn is_constructor(&self) -> bool {
      matches!(self, Self::Constructor { .. })
   }

   fn allow_new_fields(&self) -> bool {
      matches!(
         self,
         Self::Constructor {
            allow_new_fields: true
         }
      )
   }
}

struct GenerateFunctionOptions {
   name: Rc<str>,
   call_conv: FunctionCallConv,
}

struct GeneratedFunction {
   id: Opr24,
   parameter_count: u16,
}

/// What kind of expression to generate in this place.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Expression {
   /// Discard the result.
   Discarded,
   /// Use the result.
   Used,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpressionResult {
   /// The generated code leaves a value on the stack.
   Present,
   /// The generated code does not generate a value.
   Absent,
   /// The generated code does not return to the original place and unwinds the stack.
   NoReturn,
}
