//! Bytecode generation.

use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Ast, NodeId, NodeKind};
use crate::bytecode::{Chunk, Environment, Function, FunctionKind, Opcode, Opr24};
use crate::common::{Error, ErrorKind};

#[derive(Debug, Default)]
struct Scope {
   /// Mapping from variable names to stack slots.
   variables: HashMap<String, (Opr24, VariableAllocation)>,
   allocated_variable_count: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Variable {
   Global(Opr24),
   Local(Opr24),
}

/// The kind of a variable allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableAllocation {
   /// Inherit the allocation from the caller (parameter passing).
   Inherit,
   /// Allocate new storage for the variable.
   Allocate,
}

#[derive(Debug, Default)]
struct BreakableBlock {
   /// A list of offsets where `breaks` should be backpatched.
   breaks: Vec<usize>,
   start: usize,
}

pub struct CodeGenerator<'e> {
   chunk: Chunk,
   env: &'e mut Environment,
   scopes: Vec<Scope>,
   /// The next stack slot to be occupied by a variable.
   local_count: u32,
   /// The total amount of locals allocated at a time.
   allocated_local_count: u32,
   breakable_blocks: Vec<BreakableBlock>,
}

impl<'e> CodeGenerator<'e> {
   /// Constructs a new code generator with an empty chunk.
   pub fn new(module_name: Rc<str>, env: &'e mut Environment) -> Self {
      Self {
         chunk: Chunk::new(module_name),
         env,
         scopes: Vec::new(),
         local_count: 0,
         allocated_local_count: 0,
         breakable_blocks: Vec::new(),
      }
   }

   /// Creates a variable. If there is a scope on the stack, the variable is local; otherwise it
   /// is global.
   fn create_variable(
      &mut self,
      name: &str,
      allocation: VariableAllocation,
   ) -> Result<Variable, ErrorKind> {
      if !self.scopes.is_empty() {
         let slot = self.local_count;
         let slot = Opr24::new(slot).map_err(|_| ErrorKind::TooManyLocals)?;
         let scope = self.scopes.last_mut().unwrap();
         scope.variables.insert(name.to_owned(), (slot, allocation));
         self.local_count += 1;
         if allocation == VariableAllocation::Allocate {
            self.allocated_local_count += 1;
            scope.allocated_variable_count += 1;
            self.chunk.preallocate_stack_slots =
               self.chunk.preallocate_stack_slots.max(self.allocated_local_count);
         }
         Ok(Variable::Local(slot))
      } else {
         let slot = self.env.create_global(name)?;
         Ok(Variable::Global(slot))
      }
   }

   /// Performs a variable lookup. Returns the stack slot of the variable if it exists.
   /// Otherwise returns `None`.
   fn lookup_variable(&mut self, name: &str) -> Option<Variable> {
      for scope in self.scopes.iter().rev() {
         if scope.variables.contains_key(name) {
            return scope.variables.get(name).copied().map(|(slot, _)| Variable::Local(slot));
         }
      }
      self.env.get_global(name).map(Variable::Global)
   }

   /// Pushes a new scope onto the scope stack.
   fn push_scope(&mut self) {
      self.scopes.push(Default::default());
   }

   /// Pops the topmost scope off the scope stack and frees storage of any variables.
   fn pop_scope(&mut self) {
      let scope = self.scopes.pop().expect("no scopes left on the stack");
      self.local_count -= scope.variables.len() as u32;
      self.allocated_local_count -= scope.allocated_variable_count;
   }

   /// Generates a variable load instruction (GetLocal or GetGlobal).
   fn generate_variable_load(&mut self, variable: Variable) {
      self.chunk.push(match variable {
         Variable::Global(slot) => Opcode::GetGlobal(slot),
         Variable::Local(slot) => Opcode::GetLocal(slot),
      });
   }

   /// Generates a variable assign instruction (AssignLocal or AssignGlobal).
   fn generate_variable_assign(&mut self, variable: Variable) {
      self.chunk.push(match variable {
         Variable::Global(slot) => Opcode::AssignGlobal(slot),
         Variable::Local(slot) => Opcode::AssignLocal(slot),
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
   fn generate_node_list(&mut self, ast: &Ast, nodes: &[NodeId]) -> Result<(), Error> {
      for (i, &node) in nodes.iter().enumerate() {
         self.generate_node(ast, node)?;
         if i != nodes.len() - 1 {
            self.chunk.push(Opcode::Discard);
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
      if let Some(variable) = self.lookup_variable(name) {
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
            let variable = if let Some(slot) = self.lookup_variable(name) {
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
      self.generate_node(ast, right)?;
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
      self.generate_node(ast, function)?;
      let arguments = ast.children(node).unwrap();
      for &argument in arguments {
         self.generate_node(ast, argument)?;
      }
      self.chunk.push(Opcode::Call(
         arguments.len().try_into().map_err(|_| ast.error(node, ErrorKind::TooManyArguments))?,
      ));
      Ok(())
   }

   /// Generates code for a function declaration.
   fn generate_function(&mut self, ast: &Ast, node: NodeId) -> Result<(), Error> {
      let (name_node, parameters) = ast.node_pair(node);
      let parameter_list = ast.children(parameters).unwrap();
      let body = ast.children(node).unwrap();
      let name = ast.string(name_node).unwrap();

      // Create the variable before compiling the function, to allow for recursion.
      let variable = self
         .create_variable(name, VariableAllocation::Allocate)
         .map_err(|kind| ast.error(name_node, kind))?;

      let mut generator = CodeGenerator::new(Rc::clone(&self.chunk.module_name), self.env);
      // Push a scope to enforce creating local variables.
      generator.push_scope();
      // Create local variables for all the parameters.
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

      let function = Function {
         name: Rc::from(name),
         parameter_count: u16::try_from(parameter_list.len())
            .map_err(|_| ast.error(parameters, ErrorKind::TooManyParameters))?,
         kind: FunctionKind::Bytecode(Rc::new(generator.chunk)),
      };
      let function_id = self.env.create_function(function).map_err(|kind| ast.error(node, kind))?;
      self.chunk.push(Opcode::CreateClosure(function_id));
      self.generate_variable_assign(variable);

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

         NodeKind::Main => self.generate_node_list(ast, ast.children(node).unwrap())?,

         NodeKind::Do => self.generate_do(ast, node)?,
         NodeKind::If => self.generate_if(ast, node)?,
         NodeKind::While => self.generate_while(ast, node)?,
         NodeKind::Break => self.generate_break(ast, node)?,

         NodeKind::Func => self.generate_function(ast, node)?,
         NodeKind::Call => self.generate_call(ast, node)?,
         NodeKind::Return => todo!("return is NYI"),

         NodeKind::IfBranch | NodeKind::ElseBranch | NodeKind::Parameters => {
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
