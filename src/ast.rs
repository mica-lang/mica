use crate::common::{Error, ErrorKind, Location};

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
