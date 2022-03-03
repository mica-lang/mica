use std::cmp::Ordering;
use std::collections::HashMap;

use crate::ast::{Node, NodeKind};
use crate::common::{Error, ErrorKind};
use crate::value::Value;

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

   pub fn interpret(&mut self, node: &Node) -> Result<Value, Error> {
      macro_rules! binary_arithmetic {
         ($left:expr, $right:expr, $op:tt) => {{
            let left = self.interpret($left)?.number($left.location)?;
            let right = self.interpret($right)?.number($right.location)?;
            Value::Number(left $op right)
         }};
      }

      macro_rules! binary_equality {
         ($left:expr, $right:expr, $op:tt) => {{
            let left = self.interpret($left)?;
            let right = self.interpret($right)?;
            Value::from(left $op right)
         }};
      }

      fn binary_ordering(
         this: &mut Interpreter,
         left_node: &Node,
         right_node: &Node,
         check: impl FnOnce(Ordering) -> bool,
      ) -> Result<Value, Error> {
         let left = this.interpret(left_node)?;
         let right = this.interpret(right_node)?;
         let ordering = left.try_partial_cmp(&right, right_node.location)?;
         if let Some(ordering) = ordering {
            Ok(Value::from(check(ordering)))
         } else {
            Ok(Value::False)
         }
      }

      Ok(match &node.kind {
         NodeKind::Nil => Value::Nil,
         NodeKind::False => Value::False,
         NodeKind::True => Value::True,
         NodeKind::Number(x) => Value::Number(*x),
         NodeKind::String(s) => Value::String(s.clone()),

         NodeKind::Identifier(name) => self.get_variable(name).cloned().unwrap_or(Value::Nil),

         NodeKind::Negate(right) => {
            let right = self.interpret(right)?.number(node.location)?;
            Value::Number(-right)
         }
         NodeKind::Add(left, right) => binary_arithmetic!(left, right, +),
         NodeKind::Subtract(left, right) => binary_arithmetic!(left, right, -),
         NodeKind::Multiply(left, right) => binary_arithmetic!(left, right, *),
         NodeKind::Divide(left, right) => binary_arithmetic!(left, right, /),

         NodeKind::Not(right) => {
            let right = self.interpret(right)?.boolean(node.location)?;
            Value::from(!right)
         }
         NodeKind::And(left, right) => {
            let left = self.interpret(left)?;
            if left.is_truthy() {
               self.interpret(right)?
            } else {
               left
            }
         }
         NodeKind::Or(left, right) => {
            let left = self.interpret(left)?;
            if left.is_falsy() {
               self.interpret(right)?
            } else {
               left
            }
         }
         NodeKind::Equal(left, right) => binary_equality!(left, right, ==),
         NodeKind::NotEqual(left, right) => binary_equality!(left, right, !=),
         NodeKind::Less(left, right) => binary_ordering(self, left, right, |o| o.is_lt())?,
         NodeKind::Greater(left, right) => binary_ordering(self, left, right, |o| o.is_gt())?,
         NodeKind::LessEqual(left, right) => binary_ordering(self, left, right, |o| o.is_le())?,
         NodeKind::GreaterEqual(left, right) => binary_ordering(self, left, right, |o| o.is_ge())?,

         NodeKind::Assign(left, right) => {
            if let NodeKind::Identifier(name) = &left.kind {
               let value = self.interpret(right)?;
               self.set_variable(name, value.clone());
               value
            } else {
               return Err(left.error(ErrorKind::InvalidAssignment));
            }
         }

         NodeKind::Do(expressions) => {
            self.push_scope();
            let result = self.interpret_all(expressions)?;
            self.pop_scope();
            result
         }

         NodeKind::If(branches) => {
            for branch in branches {
               match &branch.kind {
                  NodeKind::IfBranch(condition, then) => {
                     self.push_scope();
                     if self.interpret(condition)?.is_truthy() {
                        let result = self.interpret_all(then);
                        self.pop_scope();
                        return result;
                     } else {
                        self.pop_scope();
                     }
                  }
                  NodeKind::ElseBranch(then) => {
                     self.push_scope();
                     let result = self.interpret_all(then);
                     self.pop_scope();
                     return result;
                  }
                  _ => unreachable!(),
               }
            }
            Value::Nil
         }
         NodeKind::IfBranch(_condition, _then) => unreachable!(),
         NodeKind::ElseBranch(_then) => unreachable!(),

         NodeKind::While(condition, then) => {
            while self.interpret(condition)?.is_truthy() {
               self.interpret_all(then)?;
            }
            Value::Nil
         }
      })
   }

   fn interpret_all(&mut self, nodes: &[Box<Node>]) -> Result<Value, Error> {
      let mut result = Value::Nil;
      for expression in nodes {
         result = self.interpret(expression)?;
      }
      Ok(result)
   }
}
