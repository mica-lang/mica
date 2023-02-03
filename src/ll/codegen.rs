//! Bytecode generation.

use std::{collections::HashSet, fmt, rc::Rc};

pub use self::traits::TraitBuilder;
use self::{control_flow::BreakableBlock, structs::StructData, variables::Locals};
use super::{bytecode::Library, gc::Memory};
use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    bytecode::{Chunk, Environment, Opcode},
    error::{LanguageError, LanguageErrorKind},
};

pub struct CodeGenerator<'e> {
    env: &'e mut Environment,
    library: &'e mut Library,
    gc: &'e mut Memory,

    chunk: Chunk,

    locals: Box<Locals>,
    breakable_blocks: Vec<BreakableBlock>,
    struct_data: Option<Box<StructData>>,

    allow_new_fields: bool,
    is_constructor: bool,
    assigned_fields: HashSet<Rc<str>>,
}

impl<'e> CodeGenerator<'e> {
    /// Constructs a new code generator with an empty chunk.
    pub fn new(
        module_name: Rc<str>,
        env: &'e mut Environment,
        library: &'e mut Library,
        gc: &'e mut Memory,
    ) -> Self {
        Self {
            env,
            library,
            gc,
            chunk: Chunk::new(module_name),

            locals: Default::default(),
            breakable_blocks: Vec::new(),
            struct_data: None,

            allow_new_fields: false,
            is_constructor: false,
            assigned_fields: HashSet::new(),
        }
    }

    /// Generates code for a list of nodes. The last node's value is the one left on the stack.
    ///
    /// If there are no nodes in the list, this is equivalent to a `nil` literal.
    fn generate_node_list(&mut self, ast: &Ast, nodes: &[NodeId]) -> Result<(), LanguageError> {
        if nodes.is_empty() {
            let _ = self.generate_nil();
        } else {
            for (i, &node) in nodes.iter().enumerate() {
                self.generate_node(
                    ast,
                    node,
                    if i != nodes.len() - 1 { Expression::Discarded } else { Expression::Used },
                )?;
            }
        }
        Ok(())
    }

    /// Generates code for a valid expression node.
    ///
    /// Nodes that are not valid expressions cause a panic.
    fn generate_node(
        &mut self,
        ast: &Ast,
        node: NodeId,
        expr: Expression,
    ) -> Result<(), LanguageError> {
        let previous_codegen_location = self.chunk.codegen_location;
        self.chunk.codegen_location = ast.location(node);
        let result = match ast.kind(node) {
            NodeKind::Empty => panic!("empty nodes must never be generated"),

            NodeKind::Nil => self.generate_nil(),
            NodeKind::False | NodeKind::True => self.generate_boolean(ast, node),
            NodeKind::Number => self.generate_number(ast, node),
            NodeKind::String => self.generate_string(ast, node),

            NodeKind::Identifier => self.generate_variable(ast, node)?,

            NodeKind::Paren => {
                let (inner, _) = ast.node_pair(node);
                self.generate_node(ast, inner, expr)?;
                ExpressionResult::Present
            }

            NodeKind::List => self.generate_list(ast, node)?,
            NodeKind::Dict => self.generate_dict(ast, node)?,
            NodeKind::Tuple => self.generate_tuple(ast, node)?,
            NodeKind::Record => self.generate_record(ast, node)?,

            NodeKind::Negate | NodeKind::Not => self.generate_unary(ast, node)?,

            NodeKind::Add
            | NodeKind::Subtract
            | NodeKind::Multiply
            | NodeKind::Divide
            | NodeKind::Equal
            | NodeKind::NotEqual
            | NodeKind::Less
            | NodeKind::Greater
            | NodeKind::LessEqual
            | NodeKind::GreaterEqual => self.generate_binary(ast, node)?,

            NodeKind::And => self.generate_and(ast, node)?,
            NodeKind::Or => self.generate_or(ast, node)?,

            NodeKind::Let => self.generate_let(ast, node, expr)?,
            NodeKind::Assign => self.generate_assignment(ast, node, expr)?,
            NodeKind::Dot => self.generate_dot(ast, node)?,
            NodeKind::Field => self.generate_field(ast, node)?,

            NodeKind::Main => {
                self.generate_node_list(ast, ast.children(node).unwrap())?;
                ExpressionResult::Present
            }

            NodeKind::Do => self.generate_do(ast, node)?,
            NodeKind::If => self.generate_if(ast, node)?,
            NodeKind::While => self.generate_while(ast, node)?,
            NodeKind::For => self.generate_for(ast, node)?,
            NodeKind::Break => self.generate_break(ast, node)?,

            NodeKind::Func => {
                let (head, _) = ast.node_pair(node);
                let (name, _) = ast.node_pair(head);
                if name != NodeId::EMPTY {
                    self.generate_function_declaration(ast, node)?
                } else {
                    self.generate_function_expression(ast, node)?
                }
            }
            NodeKind::Call => self.generate_call(ast, node)?,
            NodeKind::Return => self.generate_return(ast, node)?,

            NodeKind::Struct => self.generate_struct(ast, node)?,
            NodeKind::Impl => self.generate_impl(ast, node)?,
            NodeKind::Trait => self.generate_trait(ast, node)?,
            NodeKind::ImplAs => return Err(ast.error(node, LanguageErrorKind::AsOutsideOfImpl)),

            NodeKind::Pair
            | NodeKind::IfBranch
            | NodeKind::ElseBranch
            | NodeKind::FunctionHead
            | NodeKind::Parameters
            | NodeKind::Static
            | NodeKind::Constructor => {
                unreachable!("AST implementation detail")
            }
        };
        match (result, expr) {
            (ExpressionResult::Absent, Expression::Used) => {
                let _ = self.generate_nil();
            }
            (ExpressionResult::Present, Expression::Discarded) => {
                let _ = self.chunk.emit(Opcode::Discard);
            }
            _ => (),
        }
        self.chunk.codegen_location = previous_codegen_location;
        Ok(())
    }

    /// Generates code for the given AST.
    pub fn generate(mut self, ast: &Ast, root_node: NodeId) -> Result<Rc<Chunk>, LanguageError> {
        self.generate_node(ast, root_node, Expression::Used)?;
        self.chunk.emit(Opcode::Halt);
        Ok(Rc::new(self.chunk))
    }
}

impl<'e> fmt::Debug for CodeGenerator<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CodeGenerator").finish_non_exhaustive()
    }
}

/// What kind of expression to generate in this place.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Expression {
    /// Discard the result.
    Discarded,
    /// Use the result.
    Used,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpressionResult {
    /// The generated code leaves a value on the stack.
    Present,
    /// The generated code does not generate a value.
    Absent,
    /// The generated code does not return to the original place and unwinds the stack.
    NoReturn,
}

// The actual implementation is split into multiple files so as not to have to maintain a
// multi-thousand line behemoth.
mod assignment;
mod calls;
mod control_flow;
mod functions;
mod impls;
mod literals;
mod operators;
mod structs;
mod traits;
mod tuples;
pub mod variables;
