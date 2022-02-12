use crate::common::Location;

#[derive(Debug)]
pub enum NodeKind {
   Number(f64),

   Add(Box<Node>, Box<Node>),
   Subtract(Box<Node>, Box<Node>),
   Multiply(Box<Node>, Box<Node>),
   Divide(Box<Node>, Box<Node>),
}

#[derive(Debug)]
pub struct Node {
   pub kind: NodeKind,
   pub location: Location,
}

impl Node {
   pub fn new(location: Location, kind: NodeKind) -> Self {
      Self { kind, location }
   }

   pub fn dump_to_stdout(&self) {
      fn inner(node: &Node, level: usize) {
         for _ in 0..level {
            print!("  ");
         }
         match &node.kind {
            NodeKind::Number(x) => println!("Number {x}"),
            NodeKind::Add(left, right) => {
               println!("Add");
               inner(left, level + 1);
               inner(right, level + 1);
            }
            NodeKind::Subtract(left, right) => {
               println!("Subtract");
               inner(left, level + 1);
               inner(right, level + 1);
            }
            NodeKind::Multiply(left, right) => {
               println!("Multiply");
               inner(left, level + 1);
               inner(right, level + 1);
            }
            NodeKind::Divide(left, right) => {
               println!("Divide");
               inner(left, level + 1);
               inner(right, level + 1);
            }
         }
      }
      inner(self, 0)
   }
}
