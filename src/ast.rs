// use crate::common::{Error, ErrorKind, Location};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(u32);

impl NodeId {
   pub const EMPTY: Self = NodeId(0);
}

pub struct Ast {
   nodes: Vec<(NodeKind, (u32, u32))>,
   locations: Vec<Location>,

   data: Vec<Option<NodeData>>,
}

enum NodeData {
   Number(f64),
   String(String),
   Children(Vec<NodeId>),
}

impl Ast {
   pub fn new() -> Self {
      let mut ast = Self {
         nodes: Vec::new(),
         locations: Vec::new(),
         data: Vec::new(),
      };
      let _empty = ast.create_node(NodeKind::Empty, ());
      ast
   }

   fn create_node(&mut self, kind: NodeKind, pair: impl ToNodePair) -> NodeId {
      let id = self.nodes.len();
      self.nodes.push((kind, pair.to_node_pair()));
      self.locations.push(Location::uninit());
      self.data.push(None);
      NodeId(id as u32)
   }

   pub fn build_node(&mut self, kind: NodeKind, pair: impl ToNodePair) -> NodeBuilder<'_> {
      let node = self.create_node(kind, pair);
      NodeBuilder { ast: self, node }
   }

   pub fn kind(&self, node: NodeId) -> NodeKind {
      unsafe { self.nodes.get_unchecked(node.0 as usize).0 }
   }

   pub fn pair(&self, node: NodeId) -> (u32, u32) {
      unsafe { self.nodes.get_unchecked(node.0 as usize).1 }
   }

   pub fn location(&self, node: NodeId) -> Location {
      unsafe { *self.locations.get_unchecked(node.0 as usize) }
   }

   pub fn number(&self, node: NodeId) -> Option<f64> {
      if let &Some(NodeData::Number(n)) = unsafe { self.data.get_unchecked(node.0 as usize) } {
         return Some(n);
      }
      None
   }

   pub fn string(&self, node: NodeId) -> Option<&str> {
      if let Some(NodeData::String(s)) = unsafe { self.data.get_unchecked(node.0 as usize) } {
         return Some(s);
      }
      None
   }

   pub fn children(&self, node: NodeId) -> Option<&[NodeId]> {
      if let Some(NodeData::Children(c)) = unsafe { self.data.get_unchecked(node.0 as usize) } {
         return Some(c);
      }
      None
   }

   pub fn node_pair(&self, node: NodeId) -> (NodeId, NodeId) {
      let (left, right) = self.pair(node);
      (NodeId(left), NodeId(right))
   }

   pub fn error(&self, node: NodeId, kind: ErrorKind) -> Error {
      Error {
         kind,
         location: self.location(node),
      }
   }
}

pub struct NodeBuilder<'a> {
   ast: &'a mut Ast,
   node: NodeId,
}

impl<'a> NodeBuilder<'a> {
   pub fn with_location(self, location: Location) -> Self {
      unsafe {
         *self.ast.locations.get_unchecked_mut(self.node.0 as usize) = location;
      }
      self
   }

   pub fn with_number(self, number: f64) -> Self {
      unsafe {
         *self.ast.data.get_unchecked_mut(self.node.0 as usize) = Some(NodeData::Number(number));
      }
      self
   }

   pub fn with_string(self, string: String) -> Self {
      unsafe {
         *self.ast.data.get_unchecked_mut(self.node.0 as usize) = Some(NodeData::String(string));
      }
      self
   }

   pub fn with_children(self, children: Vec<NodeId>) -> Self {
      unsafe {
         *self.ast.data.get_unchecked_mut(self.node.0 as usize) =
            Some(NodeData::Children(children));
      }
      self
   }

   pub fn done(self) -> NodeId {
      self.node
   }
}

pub trait ToNodePair {
   fn to_node_pair(&self) -> (u32, u32);
}

impl ToNodePair for (u32, u32) {
   fn to_node_pair(&self) -> (u32, u32) {
      *self
   }
}

impl ToNodePair for (NodeId, NodeId) {
   fn to_node_pair(&self) -> (u32, u32) {
      (self.0 .0, self.1 .0)
   }
}

impl ToNodePair for NodeId {
   fn to_node_pair(&self) -> (u32, u32) {
      (self.0, 0)
   }
}

impl ToNodePair for () {
   fn to_node_pair(&self) -> (u32, u32) {
      (0, 0)
   }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NodeKind {
   Empty,

   Nil,
   False,
   True,
   Number,
   String,

   Identifier,

   Negate,
   Add,
   Subtract,
   Multiply,
   Divide,

   Not,
   And,
   Or,
   Equal,
   NotEqual,
   Less,
   Greater,
   LessEqual,
   GreaterEqual,

   Assign,

   Main,
   Do,
   If,
   IfBranch,
   ElseBranch,
   While,
   Break,

   Func,
   Parameters,
   Call,
   Return,
}

// #[derive(Debug, Clone)]
// pub struct Node {
//    pub kind: NodeKind,
//    pub location: Location,
// }

// impl Node {
//    pub fn new(location: Location, kind: NodeKind) -> Self {
//       Self { kind, location }
//    }

//    pub fn error(&self, kind: ErrorKind) -> Error {
//       Error {
//          kind,
//          location: self.location,
//       }
//    }

//    pub fn dump_to_stdout(&self) {
//       fn inner(node: &Node, level: usize) {
//          macro_rules! unary {
//             ($name:tt, $right:expr, $level:expr) => {{
//                println!($name);
//                inner($right, $level + 1);
//             }};
//          }

//          macro_rules! binary {
//             ($name:tt, $left:expr, $right:expr, $level:expr) => {{
//                println!($name);
//                inner($left, $level + 1);
//                inner($right, $level + 1);
//             }};
//          }

//          fn print_children(level: usize, nodes: &[Box<Node>]) {
//             for child in nodes {
//                inner(child, level + 1)
//             }
//          }

//          for _ in 0..level {
//             print!("  ");
//          }
//          match &node.kind {
//             NodeKind::Nil => println!("Nil"),
//             NodeKind::False => println!("False"),
//             NodeKind::True => println!("True"),
//             NodeKind::Number(x) => println!("Number {x}"),
//             NodeKind::String(s) => println!("String {s:?}"),

//             NodeKind::Identifier(i) => println!("Identifier {i}"),

//             NodeKind::Negate(right) => unary!("Negate", right, level),
//             NodeKind::And(left, right) => binary!("And", left, right, level),
//             NodeKind::Or(left, right) => binary!("Or", left, right, level),
//             NodeKind::Add(left, right) => binary!("Add", left, right, level),
//             NodeKind::Subtract(left, right) => binary!("Subtract", left, right, level),
//             NodeKind::Multiply(left, right) => binary!("Multiply", left, right, level),
//             NodeKind::Divide(left, right) => binary!("Divide", left, right, level),

//             NodeKind::Not(right) => unary!("Not", right, level),
//             NodeKind::Equal(left, right) => binary!("Equal", left, right, level),
//             NodeKind::NotEqual(left, right) => binary!("NotEqual", left, right, level),
//             NodeKind::Less(left, right) => binary!("Less", left, right, level),
//             NodeKind::Greater(left, right) => binary!("Greater", left, right, level),
//             NodeKind::LessEqual(left, right) => binary!("LessEqual", left, right, level),
//             NodeKind::GreaterEqual(left, right) => binary!("GreaterEqual", left, right, level),

//             NodeKind::Assign(left, right) => binary!("Assign", left, right, level),

//             NodeKind::Do(nodes) => {
//                println!("Do");
//                print_children(level, nodes);
//             }
//             NodeKind::If(branches) => {
//                println!("If");
//                print_children(level, branches);
//             }
//             NodeKind::IfBranch(condition, then) => {
//                println!("IfBranch");
//                inner(condition, level + 1);
//                print_children(level, then);
//             }
//             NodeKind::ElseBranch(then) => {
//                println!("ElseBranch");
//                print_children(level, then);
//             }
//             NodeKind::While(condition, then) => {
//                println!("While");
//                inner(condition, level + 1);
//                print_children(level, then);
//             }
//             NodeKind::Break(value) => {
//                println!("Break");
//                if let Some(value) = value {
//                   inner(value, level + 1);
//                }
//             }
//             NodeKind::Return(value) => {
//                println!("Return");
//                if let Some(value) = value {
//                   inner(value, level + 1);
//                }
//             }

//             NodeKind::Func {
//                name,
//                parameters,
//                body,
//             } => {
//                println!("Func {name}");
//                println!("(parameters)");
//                print_children(level, parameters);
//                println!("(body)");
//                print_children(level, body);
//             }
//             NodeKind::Call(left, arguments) => {
//                println!("Call");
//                inner(left, level + 1);
//                print_children(level, arguments);
//             }
//          }
//       }
//       inner(self, 0)
//    }
// }

use std::collections::HashMap;

use crate::common::{Error, ErrorKind, Location};
