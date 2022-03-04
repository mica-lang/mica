// use crate::common::{Error, ErrorKind, Location};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(usize);

impl NodeId {
   pub const EMPTY: Self = NodeId(0);
}

pub struct Ast {
   kinds: Vec<NodeKind>,
   pairs: Vec<(usize, usize)>,
   locations: Vec<Location>,

   numbers: HashMap<NodeId, f64>,
   strings: HashMap<NodeId, String>,
   children: HashMap<NodeId, Vec<NodeId>>,
}

impl Ast {
   pub fn new() -> Self {
      let mut ast = Self {
         kinds: Vec::new(),
         pairs: Vec::new(),
         locations: Vec::new(),

         numbers: HashMap::new(),
         strings: HashMap::new(),
         children: HashMap::new(),
      };
      let _empty = ast.create_node(NodeKind::Empty, ());
      ast
   }

   fn create_node(&mut self, kind: NodeKind, pair: impl ToNodePair) -> NodeId {
      let id = self.kinds.len();
      self.kinds.push(kind);
      self.pairs.push(pair.to_node_pair());
      self.locations.push(Location::uninit());
      NodeId(id)
   }

   pub fn build_node(&mut self, kind: NodeKind, pair: impl ToNodePair) -> NodeBuilder<'_> {
      let node = self.create_node(kind, pair);
      NodeBuilder { ast: self, node }
   }

   pub fn kind(&self, node: NodeId) -> NodeKind {
      unsafe { *self.kinds.get_unchecked(node.0) }
   }

   pub fn pair(&self, node: NodeId) -> (usize, usize) {
      unsafe { *self.pairs.get_unchecked(node.0) }
   }

   pub fn location(&self, node: NodeId) -> Location {
      unsafe { *self.locations.get_unchecked(node.0) }
   }

   pub fn number(&self, node: NodeId) -> Option<f64> {
      self.numbers.get(&node).cloned()
   }

   pub fn string(&self, node: NodeId) -> Option<&str> {
      self.strings.get(&node).map(|x| x.as_str())
   }

   pub fn children(&self, node: NodeId) -> Option<&[NodeId]> {
      self.children.get(&node).map(|x| x.as_slice())
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
   pub fn with_location(mut self, location: Location) -> Self {
      unsafe {
         *self.ast.locations.get_unchecked_mut(self.node.0) = location;
      }
      self
   }

   pub fn with_number(mut self, number: f64) -> Self {
      self.ast.numbers.insert(self.node, number);
      self
   }

   pub fn with_string(mut self, string: String) -> Self {
      self.ast.strings.insert(self.node, string);
      self
   }

   pub fn with_children(mut self, children: Vec<NodeId>) -> Self {
      self.ast.children.insert(self.node, children);
      self
   }

   pub fn done(self) -> NodeId {
      self.node
   }
}

pub trait ToNodePair {
   fn to_node_pair(&self) -> (usize, usize);
}

impl ToNodePair for (usize, usize) {
   fn to_node_pair(&self) -> (usize, usize) {
      *self
   }
}

impl ToNodePair for (NodeId, NodeId) {
   fn to_node_pair(&self) -> (usize, usize) {
      (self.0 .0, self.1 .0)
   }
}

impl ToNodePair for NodeId {
   fn to_node_pair(&self) -> (usize, usize) {
      (self.0, 0)
   }
}

impl ToNodePair for () {
   fn to_node_pair(&self) -> (usize, usize) {
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
