use std::fmt::{self, Debug};
use std::rc::Rc;

use crate::common::{Error, ErrorKind, Location};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(u32);

impl NodeId {
   pub const EMPTY: Self = NodeId(0);
}

pub struct Ast {
   module_name: Rc<str>,

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
   pub fn new(module_name: Rc<str>) -> Self {
      let mut ast = Self {
         module_name,
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
      self.locations.push(Location::UNINIT);
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
      Error::Compile {
         module_name: Rc::clone(&self.module_name),
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

/// The kind of an AST node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NodeKind {
   /// An empty node. Use `NodeId::EMPTY` to refer to an AST's empty node.
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

/// A `Debug` formatter that pretty-prints ASTs.
pub struct DumpAst<'a>(pub &'a Ast, pub NodeId);

impl fmt::Debug for DumpAst<'_> {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let DumpAst(ast, root_node) = self;

      fn write_indent(f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
         for _ in 0..level {
            f.write_str("  ")?;
         }
         Ok(())
      }

      fn dump_tree_rec(
         f: &mut fmt::Formatter<'_>,
         ast: &Ast,
         node: NodeId,
         indent: usize,
         prefix: &str,
      ) -> fmt::Result {
         write_indent(f, indent)?;
         f.write_str(prefix)?;
         write!(f, "{:?} ", ast.kind(node))?;
         if let Some(n) = ast.number(node) {
            write!(f, "{}", n)?;
         }
         if let Some(s) = ast.string(node) {
            write!(f, "{:?}", s)?;
         }
         if ast.children(node).map(|children| children.is_empty()).unwrap_or(false) {
            write!(f, " (children empty)")?;
         }
         writeln!(f)?;

         let (left, right) = ast.node_pair(node);
         if left != NodeId::EMPTY {
            dump_tree_rec(f, ast, left, indent + 1, "L: ")?;
         }
         if right != NodeId::EMPTY {
            dump_tree_rec(f, ast, right, indent + 1, "R: ")?;
         }

         if let Some(children) = ast.children(node) {
            if !children.is_empty() {
               for &child in children {
                  dump_tree_rec(f, ast, child, indent + 1, "")?;
               }
            }
         }

         Ok(())
      }

      dump_tree_rec(f, ast, *root_node, 0, "")
   }
}
