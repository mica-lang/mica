use crate::ast::{Node, NodeKind};

pub type Value = f64;

pub struct Interpreter {}

impl Interpreter {
   pub fn new() -> Self {
      Self {}
   }

   pub fn interpret(&mut self, node: &Node) -> Value {
      match &node.kind {
         NodeKind::Number(x) => *x,
         NodeKind::Add(left, right) => self.interpret(left) + self.interpret(right),
         NodeKind::Subtract(left, right) => self.interpret(left) - self.interpret(right),
         NodeKind::Multiply(left, right) => self.interpret(left) * self.interpret(right),
         NodeKind::Divide(left, right) => self.interpret(left) / self.interpret(right),
      }
   }
}
