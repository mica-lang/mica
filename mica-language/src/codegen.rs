//! Bytecode generation.

use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use crate::ast::{Ast, NodeId, NodeKind};
use crate::bytecode::{
   CaptureKind, Chunk, Environment, Function, FunctionKind, FunctionSignature, Opcode, Opr24,
   Prototype,
};
use crate::common::{Error, ErrorKind};

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

pub struct CodeGenerator<'e> {
   env: &'e mut Environment,

   chunk: Chunk,

   locals: Box<Locals>,
   breakable_blocks: Vec<BreakableBlock>,
}

impl<'e> CodeGenerator<'e> {
   /// Constructs a new code generator with an empty chunk.
   pub fn new(module_name: Rc<str>, env: &'e mut Environment) -> Self {
      Self {
         env,
         chunk: Chunk::new(module_name),

         locals: Default::default(),
         breakable_blocks: Vec::new(),
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
            self.chunk.push(Opcode::CloseLocal(variable.stack_slot));
         }
      }
   }

   /// Generates a variable load instruction (GetLocal or GetGlobal).
   fn generate_variable_load(&mut self, variable: VariablePlace) {
      self.chunk.push(match variable {
         VariablePlace::Global(slot) => Opcode::GetGlobal(slot),
         VariablePlace::Local(slot) => Opcode::GetLocal(slot),
         VariablePlace::Upvalue(slot) => Opcode::GetUpvalue(slot),
      });
   }

   /// Generates a variable assign instruction (AssignLocal or AssignGlobal).
   fn generate_variable_assign(&mut self, variable: VariablePlace) {
      self.chunk.push(match variable {
         VariablePlace::Global(slot) => Opcode::AssignGlobal(slot),
         VariablePlace::Local(slot) => Opcode::AssignLocal(slot),
         VariablePlace::Upvalue(slot) => Opcode::AssignUpvalue(slot),
      });
   }

   /// Pushes a new breakable block.
   fn push_breakable_block(&mut self) {
      let start = self.chunk.push(Opcode::Nop);
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
         self.chunk.push(Opcode::ExitBreakableBlock(1));
      }
   }

   /// Generates code for a list of nodes. The last node's value is the one left on the stack.
   ///
   /// If there are no nodes in the list, this is equivalent to a `nil` literal.
   fn generate_node_list(&mut self, ast: &Ast, nodes: &[NodeId]) -> Result<(), Error> {
      if nodes.is_empty() {
         self.generate_nil();
      } else {
         for (i, &node) in nodes.iter().enumerate() {
            self.generate_node(ast, node)?;
            if i != nodes.len() - 1 {
               self.chunk.push(Opcode::Discard);
            }
         }
      }
      Ok(())
   }

   /// Generates code for a nil literal.
   fn generate_nil(&mut self) {
      self.chunk.push(Opcode::PushNil);
   }

   /// Generates code for a boolean literal.
   fn generate_boolean(&mut self, ast: &Ast, node: NodeId) {
      self.chunk.push(match ast.kind(node) {
         NodeKind::True => Opcode::PushTrue,
         NodeKind::False => Opcode::PushFalse,
         _ => unreachable!(),
      });
   }

   /// Generates code for a number literal.
   fn generate_number(&mut self, ast: &Ast, node: NodeId) {
      self.chunk.push(Opcode::PushNumber);
      let number = ast.number(node).unwrap();
      self.chunk.push_number(number);
   }

   /// Generates code for a string literal.
   fn generate_string(&mut self, ast: &Ast, node: NodeId) {
      self.chunk.push(Opcode::PushString);
      let string = ast.string(node).unwrap();
      self.chunk.push_string(string);
   }

   /// Generates code for a unary operator.
   fn generate_unary(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (left, _) = ast.node_pair(node);
      self.generate_node(ast, left)?;
      match ast.kind(node) {
         NodeKind::Negate => self.chunk.push(Opcode::Negate),
         NodeKind::Not => self.chunk.push(Opcode::Not),
         _ => unreachable!(),
      };
      Ok(())
   }

   /// Generates code for a binary operator.
   fn generate_binary(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (left, right) = ast.node_pair(node);
      self.generate_node(ast, left)?;
      self.generate_node(ast, right)?;
      match ast.kind(node) {
         NodeKind::Negate => self.chunk.push(Opcode::Negate),

         NodeKind::Add => self.chunk.push(Opcode::Add),
         NodeKind::Subtract => self.chunk.push(Opcode::Subtract),
         NodeKind::Multiply => self.chunk.push(Opcode::Multiply),
         NodeKind::Divide => self.chunk.push(Opcode::Divide),

         NodeKind::Equal => self.chunk.push(Opcode::Equal),
         NodeKind::NotEqual => {
            self.chunk.push(Opcode::Equal);
            self.chunk.push(Opcode::Not)
         }
         NodeKind::Less => self.chunk.push(Opcode::Less),
         NodeKind::LessEqual => self.chunk.push(Opcode::LessEqual),
         NodeKind::Greater => {
            self.chunk.push(Opcode::Swap);
            self.chunk.push(Opcode::Less)
         }
         NodeKind::GreaterEqual => {
            self.chunk.push(Opcode::Swap);
            self.chunk.push(Opcode::LessEqual)
         }
         _ => unreachable!(),
      };
      Ok(())
   }

   /// Generates code for a variable lookup.
   fn generate_variable(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let name = ast.string(node).unwrap();
      if let Some(variable) = self.lookup_variable(name).map_err(|kind| ast.error(node, kind))? {
         self.generate_variable_load(variable);
         Ok(())
      } else {
         Err(ast.error(node, ErrorKind::VariableDoesNotExist(name.to_owned())))
      }
   }

   /// Generates code for an assignment.
   fn generate_assignment(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (target, value) = ast.node_pair(node);
      self.generate_node(ast, value)?;

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
            self.generate_variable_assign(variable);
         }
         _ => return Err(ast.error(target, ErrorKind::InvalidAssignment)),
      }

      Ok(())
   }

   /// Generates code for a `do..end` expression.
   fn generate_do(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let children = ast.children(node).unwrap();
      self.push_scope();
      self.generate_node_list(ast, children)?;
      self.pop_scope();
      Ok(())
   }

   /// Generates code for an `if..elif..else..end` expression.
   fn generate_if(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let branches = ast.children(node).unwrap();
      let mut jumps_to_end = Vec::new();

      for (i, &branch) in branches.iter().enumerate() {
         // We need to discard the previous branch's condition (if there was a previous branch).
         if i > 0 {
            self.chunk.push(Opcode::Discard);
         }

         let then = ast.children(branch).unwrap();
         match ast.kind(branch) {
            NodeKind::IfBranch => {
               // Generate the condition.
               let (condition, _) = ast.node_pair(branch);
               self.push_scope();
               self.generate_node(ast, condition)?;
               // Generate a Nop that is later backpatched with a ConditionalJumpForward.
               let jump = self.chunk.push(Opcode::Nop);
               self.chunk.push(Opcode::Discard); // The condition has to be discarded.
               self.generate_node_list(ast, then)?;
               self.pop_scope();
               let jump_to_end = self.chunk.push(Opcode::Nop);
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
         self.chunk.push(Opcode::Discard);
         self.chunk.push(Opcode::PushNil);
      }

      // Backpatch all jumps to end with an unconditional jump forward.
      for jump in jumps_to_end {
         self.chunk.patch(
            jump,
            Opcode::jump_forward(jump, self.chunk.len())
               .map_err(|_| ast.error(node, ErrorKind::IfExpressionTooLarge))?,
         );
      }

      Ok(())
   }

   /// Generates code for an `and` infix operator.
   fn generate_and(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (left, right) = ast.node_pair(node);
      self.generate_node(ast, left)?;
      let jump_past_right = self.chunk.push(Opcode::Nop);
      self.chunk.push(Opcode::Discard);
      self.generate_node(ast, right)?;
      self.chunk.patch(
         jump_past_right,
         Opcode::jump_forward_if_falsy(jump_past_right, self.chunk.len())
            .map_err(|_| ast.error(node, ErrorKind::OperatorRhsTooLarge))?,
      );
      Ok(())
   }

   /// Generates code for an `or` infix operator.
   fn generate_or(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (left, right) = ast.node_pair(node);
      self.generate_node(ast, left)?;
      let jump_past_right = self.chunk.push(Opcode::Nop);
      self.chunk.push(Opcode::Discard);
      self.generate_node(ast, right)?;
      self.chunk.patch(
         jump_past_right,
         Opcode::jump_forward_if_truthy(jump_past_right, self.chunk.len())
            .map_err(|_| ast.error(node, ErrorKind::OperatorRhsTooLarge))?,
      );
      Ok(())
   }

   /// Generates code for a `while..do..end` loop.
   fn generate_while(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (condition, _) = ast.node_pair(node);
      let body = ast.children(node).unwrap();

      // The outer scope, so that variables can be declared in the condition.
      self.push_scope();
      // The breakable block.
      self.push_breakable_block();

      let start = self.chunk.len();
      self.generate_node(ast, condition)?;
      let jump_to_end = self.chunk.push(Opcode::Nop);
      // Discard the condition if it's true.
      self.chunk.push(Opcode::Discard);

      self.generate_node_list(ast, body)?;
      // While loops don't yield a value.
      self.chunk.push(Opcode::Discard);

      self.chunk.push(
         Opcode::jump_backward(self.chunk.len(), start)
            .map_err(|_| ast.error(node, ErrorKind::LoopTooLarge))?,
      );
      self.chunk.patch(
         jump_to_end,
         Opcode::jump_forward_if_falsy(jump_to_end, self.chunk.len())
            .map_err(|_| ast.error(node, ErrorKind::LoopTooLarge))?,
      );
      // Discard the condition if it's false.
      self.chunk.push(Opcode::Discard);

      // Because while loops are an expression, they must produce a value. That value is `nil`.
      self.chunk.push(Opcode::PushNil);

      // `break`s produce a value (or `nil` by default), so we need to jump over the
      // fallback `PushNil`.
      self.pop_breakable_block();
      self.pop_scope();

      Ok(())
   }

   /// Generates a `break` expression.
   fn generate_break(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (right, _) = ast.node_pair(node);
      if right != NodeId::EMPTY {
         self.generate_node(ast, right)?;
      } else {
         self.generate_nil();
      }
      let jump = self.chunk.push(Opcode::Nop);
      if let Some(block) = self.breakable_blocks.last_mut() {
         block.breaks.push(jump);
      } else {
         return Err(ast.error(node, ErrorKind::BreakOutsideOfLoop));
      }
      Ok(())
   }

   /// Generates code for a function call.
   fn generate_call(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
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
            self.generate_node(ast, receiver)?;
            let arguments = ast.children(node).unwrap();
            for &argument in arguments {
               self.generate_node(ast, argument)?;
            }

            // Construct the call.
            let arity: u8 = (1 + arguments.len())
               // ↑ Add 1 for the receiver, which is also an argument.
               .try_into()
               .map_err(|_| ast.error(node, ErrorKind::TooManyArguments))?;
            let signature = FunctionSignature {
               name: Rc::clone(name),
               arity: Some(arity as u16),
            };
            let method_index =
               self.env.get_method_index(&signature).map_err(|kind| ast.error(node, kind))?;
            self.chunk.push(Opcode::CallMethod(Opr24::pack((method_index, arity))));
         }
         _ => {
            self.generate_node(ast, function)?;
            let arguments = ast.children(node).unwrap();
            for &argument in arguments {
               self.generate_node(ast, argument)?;
            }
            self.chunk.push(Opcode::Call(
               arguments
                  .len()
                  .try_into()
                  .map_err(|_| ast.error(node, ErrorKind::TooManyArguments))?,
            ));
         }
      }
      Ok(())
   }

   /// Generates code for a bare dot call (without parentheses).
   fn generate_dot(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (left, method) = ast.node_pair(node);
      self.generate_node(ast, left)?;

      if ast.kind(method) != NodeKind::Identifier {
         return Err(ast.error(method, ErrorKind::InvalidMethodName));
      }
      let signature = FunctionSignature {
         name: Rc::clone(ast.string(method).unwrap()),
         arity: Some(1),
      };
      let method_index =
         self.env.get_method_index(&signature).map_err(|kind| ast.error(node, kind))?;
      self.chunk.push(Opcode::CallMethod(Opr24::pack((method_index, 1))));

      Ok(())
   }

   fn generate_function(
      &mut self,
      ast: &Ast,
      node: NodeId,
      GenerateFunctionOptions { name, self_visible }: GenerateFunctionOptions,
   ) -> Result<GeneratedFunction, Error> {
      let (_, parameters) = ast.node_pair(node);
      let parameter_list = ast.children(parameters).unwrap();
      let body = ast.children(node).unwrap();

      let mut generator = CodeGenerator::new(Rc::clone(&self.chunk.module_name), self.env);
      // NOTE(liquidev): Hopefully the allocation from this mem::take gets optimized out.
      generator.locals.parent = Some(mem::take(&mut self.locals));
      // Push a scope to enforce creating local variables.
      generator.push_scope();
      // Create local variables for all the parameters.
      // Note that in bare functions, the function itself acts as the `self` parameter.
      generator
         .create_variable(
            if self_visible {
               "self"
            } else {
               "<this function>"
            },
            VariableAllocation::Inherit,
         )
         .unwrap();
      for &parameter in parameter_list {
         let parameter_name = ast.string(parameter).unwrap();
         generator
            .create_variable(parameter_name, VariableAllocation::Inherit)
            .map_err(|kind| ast.error(parameter, kind))?;
      }
      // Generate the body.
      generator.generate_node_list(ast, body)?;
      generator.pop_scope();
      generator.chunk.push(Opcode::Return);
      self.locals = generator.locals.parent.take().unwrap();

      let parameter_count = u16::try_from(parameter_list.len())
         .map_err(|_| ast.error(parameters, ErrorKind::TooManyParameters))?;
      let function = Function {
         name,
         parameter_count: Some(parameter_count),
         kind: FunctionKind::Bytecode {
            chunk: Rc::new(generator.chunk),
            captured_locals: generator.locals.captures,
         },
      };
      let function_id = self.env.create_function(function).map_err(|kind| ast.error(node, kind))?;

      Ok(GeneratedFunction {
         id: function_id,
         parameter_count,
      })
   }

   /// Generates code for a bare function declaration.
   fn generate_function_declaration(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (name_node, parameters) = ast.node_pair(node);
      let name = ast.string(name_node).unwrap();

      if ast.node_pair(parameters).0 != NodeId::EMPTY {
         return Err(ast.error(
            ast.node_pair(parameters).0,
            ErrorKind::FunctionKindOutsideImpl,
         ));
      }

      // Create the variable before compiling the function, to allow for recursion.
      let variable = self
         .create_variable(name, VariableAllocation::Allocate)
         .map_err(|kind| ast.error(name_node, kind))?;

      let function = self.generate_function(
         ast,
         node,
         GenerateFunctionOptions {
            name: Rc::clone(name),
            self_visible: false,
         },
      )?;

      self.chunk.push(Opcode::CreateClosure(function.id));
      self.generate_variable_assign(variable);
      self.chunk.push(Opcode::Discard);
      self.generate_nil();

      Ok(())
   }

   /// Generates code for a function expression.
   fn generate_function_expression(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (_, parameters) = ast.node_pair(node);
      if ast.node_pair(parameters).0 != NodeId::EMPTY {
         return Err(ast.error(
            ast.node_pair(parameters).0,
            ErrorKind::FunctionKindOutsideImpl,
         ));
      }
      let function = self.generate_function(
         ast,
         node,
         GenerateFunctionOptions {
            name: Rc::from("<anonymous>"),
            self_visible: false,
         },
      )?;
      self.chunk.push(Opcode::CreateClosure(function.id));
      Ok(())
   }

   /// Generates code for a struct declaration.
   fn generate_struct(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (name, _) = ast.node_pair(node);
      let name = ast.string(name).unwrap();

      self.chunk.push(Opcode::CreateType);
      self.chunk.push_string(name);
      let variable = self
         .create_variable(name, VariableAllocation::Allocate)
         .map_err(|kind| ast.error(node, kind))?;
      self.generate_variable_assign(variable);
      self.chunk.push(Opcode::Discard);
      self.generate_nil();

      Ok(())
   }

   /// Generates code for an `impl` block.
   fn generate_impl(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (implementee, _) = ast.node_pair(node);
      let items = ast.children(node).unwrap();

      let mut proto = Prototype::default();

      for &item in items {
         match ast.kind(item) {
            NodeKind::Func => {
               let (name_node, parameters) = ast.node_pair(item);
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
                     self_visible: match ast.kind(kind) {
                        NodeKind::Empty | NodeKind::Static => true,
                        NodeKind::Constructor => todo!("constructors are NYI"),
                        _ => unreachable!(),
                     },
                  },
               )?;
               let map = match ast.kind(kind) {
                  NodeKind::Empty => &mut proto.instance,
                  NodeKind::Static | NodeKind::Constructor => &mut proto.statics,
                  _ => unreachable!(),
               };
               let signature = FunctionSignature {
                  name,
                  arity: Some(1 + function.parameter_count),
               };
               let method_id =
                  self.env.get_method_index(&signature).map_err(|kind| ast.error(item, kind))?;
               if map.contains_key(&method_id) {
                  return Err(ast.error(
                     name_node,
                     ErrorKind::MethodAlreadyImplemented(FunctionSignature {
                        arity: signature.arity.map(|x| x - 1),
                        ..signature
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
      self.generate_node(ast, implementee)?;
      self.chunk.push(Opcode::Implement(proto_id));

      Ok(())
   }

   /// Generates code for a single node.
   fn generate_node(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let previous_codegen_location = self.chunk.codegen_location;
      self.chunk.codegen_location = ast.location(node);
      match ast.kind(node) {
         NodeKind::Empty => panic!("empty nodes must never be generated"),

         NodeKind::Nil => self.generate_nil(),
         NodeKind::False | NodeKind::True => self.generate_boolean(ast, node),
         NodeKind::Number => self.generate_number(ast, node),
         NodeKind::String => self.generate_string(ast, node),

         NodeKind::Identifier => self.generate_variable(ast, node)?,

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

         NodeKind::Assign => self.generate_assignment(ast, node)?,
         NodeKind::Dot => self.generate_dot(ast, node)?,

         NodeKind::Main => self.generate_node_list(ast, ast.children(node).unwrap())?,

         NodeKind::Do => self.generate_do(ast, node)?,
         NodeKind::If => self.generate_if(ast, node)?,
         NodeKind::While => self.generate_while(ast, node)?,
         NodeKind::Break => self.generate_break(ast, node)?,

         NodeKind::Func => {
            if ast.node_pair(node).0 != NodeId::EMPTY {
               self.generate_function_declaration(ast, node)?;
            } else {
               self.generate_function_expression(ast, node)?;
            }
         }
         NodeKind::Call => self.generate_call(ast, node)?,
         NodeKind::Return => todo!("return is NYI"),

         NodeKind::Struct => self.generate_struct(ast, node)?,
         NodeKind::Impl => self.generate_impl(ast, node)?,

         | NodeKind::IfBranch
         | NodeKind::ElseBranch
         | NodeKind::Parameters
         | NodeKind::Static
         | NodeKind::Constructor => {
            unreachable!("AST implementation detail")
         }
      }
      self.chunk.codegen_location = previous_codegen_location;
      Ok(())
   }

   /// Generates code for the given AST.
   pub fn generate(mut self, ast: &Ast, root_node: NodeId) -> Result<Rc<Chunk>, Error> {
      self.generate_node(ast, root_node)?;
      self.chunk.push(Opcode::Halt);
      Ok(Rc::new(self.chunk))
   }
}

struct GenerateFunctionOptions {
   name: Rc<str>,
   self_visible: bool,
}

struct GeneratedFunction {
   id: Opr24,
   parameter_count: u16,
}
