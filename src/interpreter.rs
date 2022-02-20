use std::cmp::Ordering;

use crate::ast::{Node, NodeKind};
use crate::common::Error;
use crate::value::Value;

pub struct Interpreter {}

impl Interpreter {
   pub fn new() -> Self {
      Self {}
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
         NodeKind::Number(x) => Value::Number(*x),
         NodeKind::String(s) => Value::String(s.clone()),

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
         NodeKind::Equal(left, right) => binary_equality!(left, right, ==),
         NodeKind::NotEqual(left, right) => binary_equality!(left, right, !=),
         NodeKind::Less(left, right) => binary_ordering(self, left, right, |o| o.is_lt())?,
         NodeKind::Greater(left, right) => binary_ordering(self, left, right, |o| o.is_gt())?,
         NodeKind::LessEqual(left, right) => binary_ordering(self, left, right, |o| o.is_le())?,
         NodeKind::GreaterEqual(left, right) => binary_ordering(self, left, right, |o| o.is_ge())?,
      })
   }
}
