use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::{Error, ErrorKind};
use crate::value::{Function, Value};

#[derive(Default)]
struct Scope {
   variables: HashMap<String, Value>,
}

pub struct Interpreter {
   scopes: Vec<Scope>,
}

impl Interpreter {
   pub fn new() -> Self {
      Self {
         scopes: vec![Default::default()],
      }
   }

   fn top_scope_mut(&mut self) -> &mut Scope {
      self.scopes.last_mut().unwrap()
   }

   pub fn set_variable(&mut self, name: &str, value: Value) {
      for scope in self.scopes.iter_mut().rev() {
         if scope.variables.contains_key(name) {
            scope.variables.insert(name.to_owned(), value);
            return;
         }
      }
      self.top_scope_mut().variables.insert(name.to_owned(), value);
   }

   pub fn get_variable(&mut self, name: &str) -> Option<&Value> {
      for scope in self.scopes.iter().rev() {
         if scope.variables.contains_key(name) {
            return scope.variables.get(name);
         }
      }
      None
   }

   fn push_scope(&mut self) {
      self.scopes.push(Default::default());
   }

   fn pop_scope(&mut self) {
      self.scopes.pop();
   }

   pub fn interpret(&mut self, ast: &Ast, node: NodeId) -> Result<Value, Error> {
      macro_rules! binary_arithmetic {
         ($op:tt) => {{
            let (left, right) = ast.node_pair(node);
            let left = self.interpret(ast, left)?.number(ast.location(left))?;
            let right = self.interpret(ast, right)?.number(ast.location(right))?;
            Value::Number(left $op right)
         }};
      }

      macro_rules! binary_equality {
         ($op:tt) => {{
            let (left, right) = ast.node_pair(node);
            let left = self.interpret(ast, left)?;
            let right = self.interpret(ast, right)?;
            Value::from(left $op right)
         }};
      }

      fn binary_ordering(
         this: &mut Interpreter,
         ast: &Ast,
         node: NodeId,
         check: impl FnOnce(Ordering) -> bool,
      ) -> Result<Value, Error> {
         let (left_node, right_node) = ast.node_pair(node);
         let left = this.interpret(ast, left_node)?;
         let right = this.interpret(ast, right_node)?;
         let ordering = left.try_partial_cmp(&right, ast.location(right_node))?;
         if let Some(ordering) = ordering {
            Ok(Value::from(check(ordering)))
         } else {
            Ok(Value::False)
         }
      }

      Ok(match ast.kind(node) {
         NodeKind::Empty => panic!("cannot evaluate Empty node"),
         NodeKind::Nil => Value::Nil,
         NodeKind::False => Value::False,
         NodeKind::True => Value::True,
         NodeKind::Number => Value::Number(ast.number(node).unwrap()),
         NodeKind::String => Value::String(ast.string(node).map(|s| s.to_owned()).unwrap()),

         NodeKind::Identifier => {
            let name = ast.string(node).unwrap();
            self.get_variable(name).cloned().unwrap_or(Value::Nil)
         }

         NodeKind::Negate => {
            let (_, right) = ast.node_pair(node);
            let right = self.interpret(ast, right)?.number(ast.location(node))?;
            Value::Number(-right)
         }
         NodeKind::Add => binary_arithmetic!(+),
         NodeKind::Subtract => binary_arithmetic!(-),
         NodeKind::Multiply => binary_arithmetic!(*),
         NodeKind::Divide => binary_arithmetic!(/),

         NodeKind::Not => {
            let (_, right) = ast.node_pair(node);
            let right = self.interpret(ast, right)?.boolean(ast.location(node))?;
            Value::from(!right)
         }
         NodeKind::And => {
            let (left, right) = ast.node_pair(node);
            let left = self.interpret(ast, left)?;
            if left.is_truthy() {
               self.interpret(ast, right)?
            } else {
               left
            }
         }
         NodeKind::Or => {
            let (left, right) = ast.node_pair(node);
            let left = self.interpret(ast, left)?;
            if left.is_falsy() {
               self.interpret(ast, right)?
            } else {
               left
            }
         }
         NodeKind::Equal => binary_equality!(==),
         NodeKind::NotEqual => binary_equality!(!=),
         NodeKind::Less => binary_ordering(self, ast, node, |o| o.is_lt())?,
         NodeKind::Greater => binary_ordering(self, ast, node, |o| o.is_gt())?,
         NodeKind::LessEqual => binary_ordering(self, ast, node, |o| o.is_le())?,
         NodeKind::GreaterEqual => binary_ordering(self, ast, node, |o| o.is_ge())?,

         NodeKind::Assign => {
            let (left, right) = ast.node_pair(node);
            if ast.kind(left) == NodeKind::Identifier {
               let value = self.interpret(ast, right)?;
               let name = ast.string(left).unwrap();
               self.set_variable(name, value.clone());
               value
            } else {
               return Err(ast.error(left, ErrorKind::InvalidAssignment));
            }
         }

         NodeKind::Main => {
            let nodes = ast.children(node).unwrap();
            self.interpret_all(ast, nodes)?
         }

         NodeKind::Do => {
            self.push_scope();
            let expressions = ast.children(node).unwrap();
            let result = self.interpret_all(ast, expressions)?;
            self.pop_scope();
            result
         }

         NodeKind::If => {
            let branches = ast.children(node).unwrap();
            for &branch in branches {
               match ast.kind(branch) {
                  NodeKind::IfBranch => {
                     let (condition, _) = ast.node_pair(branch);
                     let then = ast.children(branch).unwrap();
                     self.push_scope();
                     if self.interpret(ast, condition)?.is_truthy() {
                        let result = self.interpret_all(ast, then);
                        self.pop_scope();
                        return result;
                     } else {
                        self.pop_scope();
                     }
                  }
                  NodeKind::ElseBranch => {
                     let then = ast.children(branch).unwrap();
                     self.push_scope();
                     let result = self.interpret_all(ast, then);
                     self.pop_scope();
                     return result;
                  }
                  _ => unreachable!(),
               }
            }
            Value::Nil
         }
         NodeKind::IfBranch => unreachable!(),
         NodeKind::ElseBranch => unreachable!(),

         NodeKind::While => {
            let (condition, _) = ast.node_pair(node);
            let then = ast.children(node).unwrap();
            while self.interpret(ast, condition)?.is_truthy() {
               match self.interpret_all(ast, then) {
                  Ok(_) => (),
                  Err(Error {
                     kind: ErrorKind::Break(value),
                     ..
                  }) => return Ok(value),
                  Err(error) => return Err(error),
               }
            }
            Value::Nil
         }

         NodeKind::Func => {
            let (name, parameters) = ast.node_pair(node);
            let parameters = ast.children(parameters).unwrap();
            let body = ast.children(node).unwrap();
            let value = Value::Function(Rc::new(Function {
               parameters: parameters
                  .iter()
                  .map(|&node| ast.string(node).unwrap().to_owned())
                  .collect(),
               body: body.to_owned(),
            }));
            self.set_variable(ast.string(name).unwrap(), value);
            Value::Nil
         }
         NodeKind::Parameters => unreachable!(),
         NodeKind::Call => {
            let (left, _) = ast.node_pair(node);
            let function = self.interpret(ast, left)?;
            let function = if let Value::Function(function) = function {
               function
            } else {
               return Err(ast.error(left, ErrorKind::CannotCall(function.typ())));
            };
            let mut interpreter = Interpreter::new();
            let arguments = ast.children(node).unwrap();
            for (&argument, parameter_name) in arguments.iter().zip(&function.parameters) {
               let argument = self.interpret(ast, argument)?;
               interpreter.set_variable(parameter_name, argument)
            }
            match interpreter.interpret_all(ast, &function.body) {
               Ok(value) => value,
               Err(Error {
                  kind: ErrorKind::Return(value),
                  ..
               }) => return Ok(value),
               Err(error) => return Err(error),
            }
         }

         NodeKind::Break => {
            let (right, _) = ast.node_pair(node);
            let value = if right != NodeId::EMPTY {
               self.interpret(ast, right)?
            } else {
               Value::Nil
            };
            return Err(ast.error(node, ErrorKind::Break(value)));
         }
         NodeKind::Return => {
            let (right, _) = ast.node_pair(node);
            let value = if right != NodeId::EMPTY {
               self.interpret(ast, right)?
            } else {
               Value::Nil
            };
            return Err(ast.error(node, ErrorKind::Return(value)));
         }
      })
   }

   fn interpret_all(&mut self, ast: &Ast, nodes: &[NodeId]) -> Result<Value, Error> {
      let mut result = Value::Nil;
      for &expression in nodes {
         result = self.interpret(ast, expression)?;
      }
      Ok(result)
   }
}
