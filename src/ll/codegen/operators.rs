//! Code generation for unary and binary operators.

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    bytecode::Opcode,
    error::LanguageError,
};

impl<'e> CodeGenerator<'e> {
    /// Generates code for a unary operator.
    pub(super) fn generate_unary(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (left, _) = ast.node_pair(node);
        self.generate_node(ast, left, Expression::Used)?;
        match ast.kind(node) {
            NodeKind::Negate => self.chunk.emit(Opcode::Negate),
            NodeKind::Not => self.chunk.emit(Opcode::Not),
            _ => unreachable!(),
        };
        Ok(ExpressionResult::Present)
    }

    /// Generates code for a binary operator.
    pub(super) fn generate_binary(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (left, right) = ast.node_pair(node);
        self.generate_node(ast, left, Expression::Used)?;
        self.generate_node(ast, right, Expression::Used)?;
        match ast.kind(node) {
            NodeKind::Negate => self.chunk.emit(Opcode::Negate),

            NodeKind::Add => self.chunk.emit(Opcode::Add),
            NodeKind::Subtract => self.chunk.emit(Opcode::Subtract),
            NodeKind::Multiply => self.chunk.emit(Opcode::Multiply),
            NodeKind::Divide => self.chunk.emit(Opcode::Divide),

            NodeKind::Equal => self.chunk.emit(Opcode::Equal),
            NodeKind::NotEqual => {
                self.chunk.emit(Opcode::Equal);
                self.chunk.emit(Opcode::Not)
            }
            NodeKind::Less => self.chunk.emit(Opcode::Less),
            NodeKind::LessEqual => self.chunk.emit(Opcode::LessEqual),
            NodeKind::Greater => {
                self.chunk.emit(Opcode::Swap);
                self.chunk.emit(Opcode::Less)
            }
            NodeKind::GreaterEqual => {
                self.chunk.emit(Opcode::Swap);
                self.chunk.emit(Opcode::LessEqual)
            }
            _ => unreachable!(),
        };
        Ok(ExpressionResult::Present)
    }
}
