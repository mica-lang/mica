use crate::common::{Error, ErrorKind, Location};

#[derive(Debug, Clone)]
pub enum NodeKind {
   Nil,
   False,
   True,
   Number(f64),
   String(String),

   Identifier(String),

   Negate(Box<Node>),
   Add(Box<Node>, Box<Node>),
   Subtract(Box<Node>, Box<Node>),
   Multiply(Box<Node>, Box<Node>),
   Divide(Box<Node>, Box<Node>),

   Not(Box<Node>),
   And(Box<Node>, Box<Node>),
   Or(Box<Node>, Box<Node>),
   Equal(Box<Node>, Box<Node>),
   NotEqual(Box<Node>, Box<Node>),
   Less(Box<Node>, Box<Node>),
   Greater(Box<Node>, Box<Node>),
   LessEqual(Box<Node>, Box<Node>),
   GreaterEqual(Box<Node>, Box<Node>),

   Assign(Box<Node>, Box<Node>),

   Do(Vec<Box<Node>>),
   If(Vec<Box<Node>>),
   IfBranch(Box<Node>, Vec<Box<Node>>),
   ElseBranch(Vec<Box<Node>>),
   While(Box<Node>, Vec<Box<Node>>),
   Break(Option<Box<Node>>),

   Func {
      name: String,
      parameters: Vec<Box<Node>>,
      body: Vec<Box<Node>>,
   },
   Call(Box<Node>, Vec<Box<Node>>),
   Return(Option<Box<Node>>),
}

#[derive(Debug, Clone)]
pub struct Node {
   pub kind: NodeKind,
   pub location: Location,
}

impl Node {
   pub fn new(location: Location, kind: NodeKind) -> Self {
      Self { kind, location }
   }

   pub fn error(&self, kind: ErrorKind) -> Error {
      Error {
         kind,
         location: self.location,
      }
   }

   pub fn dump_to_stdout(&self) {
      fn inner(node: &Node, level: usize) {
         macro_rules! unary {
            ($name:tt, $right:expr, $level:expr) => {{
               println!($name);
               inner($right, $level + 1);
            }};
         }

         macro_rules! binary {
            ($name:tt, $left:expr, $right:expr, $level:expr) => {{
               println!($name);
               inner($left, $level + 1);
               inner($right, $level + 1);
            }};
         }

         fn print_children(level: usize, nodes: &[Box<Node>]) {
            for child in nodes {
               inner(child, level + 1)
            }
         }

         for _ in 0..level {
            print!("  ");
         }
         match &node.kind {
            NodeKind::Nil => println!("Nil"),
            NodeKind::False => println!("False"),
            NodeKind::True => println!("True"),
            NodeKind::Number(x) => println!("Number {x}"),
            NodeKind::String(s) => println!("String {s:?}"),

            NodeKind::Identifier(i) => println!("Identifier {i}"),

            NodeKind::Negate(right) => unary!("Negate", right, level),
            NodeKind::And(left, right) => binary!("And", left, right, level),
            NodeKind::Or(left, right) => binary!("Or", left, right, level),
            NodeKind::Add(left, right) => binary!("Add", left, right, level),
            NodeKind::Subtract(left, right) => binary!("Subtract", left, right, level),
            NodeKind::Multiply(left, right) => binary!("Multiply", left, right, level),
            NodeKind::Divide(left, right) => binary!("Divide", left, right, level),

            NodeKind::Not(right) => unary!("Not", right, level),
            NodeKind::Equal(left, right) => binary!("Equal", left, right, level),
            NodeKind::NotEqual(left, right) => binary!("NotEqual", left, right, level),
            NodeKind::Less(left, right) => binary!("Less", left, right, level),
            NodeKind::Greater(left, right) => binary!("Greater", left, right, level),
            NodeKind::LessEqual(left, right) => binary!("LessEqual", left, right, level),
            NodeKind::GreaterEqual(left, right) => binary!("GreaterEqual", left, right, level),

            NodeKind::Assign(left, right) => binary!("Assign", left, right, level),

            NodeKind::Do(nodes) => {
               println!("Do");
               print_children(level, nodes);
            }
            NodeKind::If(branches) => {
               println!("If");
               print_children(level, branches);
            }
            NodeKind::IfBranch(condition, then) => {
               println!("IfBranch");
               inner(condition, level + 1);
               print_children(level, then);
            }
            NodeKind::ElseBranch(then) => {
               println!("ElseBranch");
               print_children(level, then);
            }
            NodeKind::While(condition, then) => {
               println!("While");
               inner(condition, level + 1);
               print_children(level, then);
            }
            NodeKind::Break(value) => {
               println!("Break");
               if let Some(value) = value {
                  inner(value, level + 1);
               }
            }
            NodeKind::Return(value) => {
               println!("Return");
               if let Some(value) = value {
                  inner(value, level + 1);
               }
            }

            NodeKind::Func {
               name,
               parameters,
               body,
            } => {
               println!("Func {name}");
               println!("(parameters)");
               print_children(level, parameters);
               println!("(body)");
               print_children(level, body);
            }
            NodeKind::Call(left, arguments) => {
               println!("Call");
               inner(left, level + 1);
               print_children(level, arguments);
            }
         }
      }
      inner(self, 0)
   }
}
