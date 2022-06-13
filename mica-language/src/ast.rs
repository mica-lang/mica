//! The representation of Mica's abstract syntax tree.

use std::fmt::{self, Debug};
use std::rc::Rc;

use crate::common::{Error, ErrorKind, Location};

/// A lightweight handle to a node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct NodeId(u32);

impl NodeId {
   /// Represents the empty node in a syntax tree.
   pub const EMPTY: Self = NodeId(0);
}

/// An abstract syntax tree.
pub struct Ast {
   module_name: Rc<str>,

   nodes: Vec<(NodeKind, (u32, u32))>,
   locations: Vec<Location>,

   data: Vec<Option<NodeData>>,
}

/// Data a node can carry around with it.
enum NodeData {
   Number(f64),
   String(Rc<str>),
   Children(Vec<NodeId>),
}

impl Ast {
   /// Creates a new, empty syntax tree. The module name is used for error messages.
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

   /// Appends a node to the syntax tree.
   fn create_node(&mut self, kind: NodeKind, pair: impl ToNodePair) -> NodeId {
      let id = self.nodes.len();
      self.nodes.push((kind, pair.to_node_pair()));
      self.locations.push(Location::UNINIT);
      self.data.push(None);
      NodeId(id as u32)
   }

   /// Begins building a node in this syntax tree.
   pub fn build_node(&mut self, kind: NodeKind, pair: impl ToNodePair) -> NodeBuilder<'_> {
      let node = self.create_node(kind, pair);
      NodeBuilder { ast: self, node }
   }

   /// Returns the kind of a node.
   pub fn kind(&self, node: NodeId) -> NodeKind {
      unsafe { self.nodes.get_unchecked(node.0 as usize).0 }
   }

   /// Returns the raw pair of a node.
   pub fn pair(&self, node: NodeId) -> (u32, u32) {
      unsafe { self.nodes.get_unchecked(node.0 as usize).1 }
   }

   fn data(&self, node: NodeId) -> Option<&NodeData> {
      unsafe { self.data.get_unchecked(node.0 as usize).as_ref() }
   }

   /// Returns the source location of a node.
   pub fn location(&self, node: NodeId) -> Location {
      unsafe { *self.locations.get_unchecked(node.0 as usize) }
   }

   /// Returns the number data of a node, or `None` if the node carries a different type of data.
   pub fn number(&self, node: NodeId) -> Option<f64> {
      if let Some(&NodeData::Number(n)) = self.data(node) {
         return Some(n);
      }
      None
   }

   /// Returns the string data of a node, or `None` if the node carries a different type of data.
   pub fn string(&self, node: NodeId) -> Option<&Rc<str>> {
      if let Some(NodeData::String(s)) = self.data(node) {
         return Some(s);
      }
      None
   }

   /// Returns the children data of a node, or `None` if the node carries a different type of data.
   pub fn children(&self, node: NodeId) -> Option<&[NodeId]> {
      if let Some(NodeData::Children(c)) = self.data(node) {
         return Some(c);
      }
      None
   }

   /// Returns the pair of nodes this node points to.
   pub fn node_pair(&self, node: NodeId) -> (NodeId, NodeId) {
      let (left, right) = self.pair(node);
      (NodeId(left), NodeId(right))
   }

   /// Constructs a compile error at the given node.
   pub fn error(&self, node: NodeId, kind: ErrorKind) -> Error {
      Error::Compile {
         module_name: Rc::clone(&self.module_name),
         kind,
         location: self.location(node),
      }
   }
}

/// A node builder.
pub struct NodeBuilder<'a> {
   ast: &'a mut Ast,
   node: NodeId,
}

impl<'a> NodeBuilder<'a> {
   /// Sets the location of the node.
   pub fn with_location(self, location: Location) -> Self {
      unsafe {
         *self.ast.locations.get_unchecked_mut(self.node.0 as usize) = location;
      }
      self
   }

   /// Sets the number data of the node.
   pub fn with_number(self, number: f64) -> Self {
      unsafe {
         *self.ast.data.get_unchecked_mut(self.node.0 as usize) = Some(NodeData::Number(number));
      }
      self
   }

   /// Sets the string data of the node.
   pub fn with_string(self, string: Rc<str>) -> Self {
      unsafe {
         *self.ast.data.get_unchecked_mut(self.node.0 as usize) = Some(NodeData::String(string));
      }
      self
   }

   /// Sets the children data of the node.
   pub fn with_children(self, children: Vec<NodeId>) -> Self {
      unsafe {
         *self.ast.data.get_unchecked_mut(self.node.0 as usize) =
            Some(NodeData::Children(children));
      }
      self
   }

   /// Finishes building the node and returns a handle to it.
   pub fn done(self) -> NodeId {
      self.node
   }
}

/// Implemented by all types convertible to a raw node pair.
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

   /// A `nil` literal.
   Nil,
   /// A `false` literal.
   False,
   /// A `true` literal.
   True,
   /// A number literal. Must contain number data.
   Number,
   /// A string literal. Must contain string data.
   String,

   /// An identifier. Must contain string data.
   Identifier,

   /// A list literal.
   List,
   /// A dict literal.
   Dict,
   /// A `key: value` pair in a dict literal.
   DictPair,

   /// Negation operator (prefix `-`).
   Negate,
   /// Addition operator `+`.
   Add,
   /// Subtraction operator `-`.
   Subtract,
   /// Multiplication operator `*`.
   Multiply,
   /// Division operator `/`.
   Divide,

   /// Boolean NOT `!`.
   Not,
   /// Boolean AND `and`.
   And,
   /// Boolean OR `or`.
   Or,
   /// Equality operator `==`.
   Equal,
   /// Inequality operator `!=`.
   NotEqual,
   /// Less-than operator `<`.
   Less,
   /// Greater-than operator `>`.
   Greater,
   /// Less-than-or-equal-to operator `<=`.
   LessEqual,
   /// Greater-than-or-equal-to operator `>=`.
   GreaterEqual,

   /// Assignment operator `=`.
   Assign,
   /// Method call operator `.`.
   Dot,
   /// Field reference `@x`.
   Field,

   /// Top-level code in a module.
   Main,
   /// `do` expression.
   Do,
   /// `if..do..elif..else..end` expression.
   If,
   /// An `if` or `elif` branch of an `if` expression.
   IfBranch,
   /// The `else` branch of an `if` expression.
   ElseBranch,
   /// `while` loop.
   While,
   /// `break` expression.
   Break,

   /// Function (item or anonymous).
   Func,
   /// Function parameters (the RHS of `Function`).
   Parameters,
   /// `static` keyword (the LHS of `Parameters`).
   Static,
   /// `constructor` keyword (the LHS of `Parameters`).
   Constructor,
   /// A function call.
   Call,
   /// `return` expression.
   Return,

   /// A struct declaration.
   Struct,
   /// An `impl` block.
   Impl,
   /// A trait declaration.
   Trait,
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
