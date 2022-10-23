//! Code generation for various kinds of literals.

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    bytecode::{Opcode, Opr24},
    error::{Error, ErrorKind},
};

impl<'e> CodeGenerator<'e> {
    /// Generates code for a nil literal.
    pub(super) fn generate_nil(&mut self) -> ExpressionResult {
        self.chunk.emit(Opcode::PushNil);
        ExpressionResult::Present
    }

    /// Generates code for a boolean literal.
    pub(super) fn generate_boolean(&mut self, ast: &Ast, node: NodeId) -> ExpressionResult {
        self.chunk.emit(match ast.kind(node) {
            NodeKind::True => Opcode::PushTrue,
            NodeKind::False => Opcode::PushFalse,
            _ => unreachable!(),
        });
        ExpressionResult::Present
    }

    /// Generates code for a number literal.
    pub(super) fn generate_number(&mut self, ast: &Ast, node: NodeId) -> ExpressionResult {
        self.chunk.emit(Opcode::PushNumber);
        let number = ast.number(node).unwrap();
        self.chunk.emit_number(number);
        ExpressionResult::Present
    }

    /// Generates code for a string literal.
    pub(super) fn generate_string(&mut self, ast: &Ast, node: NodeId) -> ExpressionResult {
        self.chunk.emit(Opcode::PushString);
        let string = ast.string(node).unwrap();
        self.chunk.emit_string(string);
        ExpressionResult::Present
    }

    /// Generates code for a list literal.
    pub(super) fn generate_list(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        let children = ast.children(node).unwrap();
        let len = Opr24::try_from(children.len())
            .map_err(|_| ast.error(node, ErrorKind::ListIsTooLong))?;
        for &child in children {
            self.generate_node(ast, child, Expression::Used)?;
        }
        self.chunk.emit((Opcode::CreateList, len));
        Ok(ExpressionResult::Present)
    }

    /// Generates code for a dict literal.
    pub(super) fn generate_dict(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        let children = ast.children(node).unwrap();
        let len = Opr24::try_from(children.len())
            .map_err(|_| ast.error(node, ErrorKind::DictIsTooLarge))?;
        for &child in children {
            let (key, value) = ast.node_pair(child);
            self.generate_node(ast, key, Expression::Used)?;
            self.generate_node(ast, value, Expression::Used)?;
        }
        self.chunk.emit((Opcode::CreateDict, len));
        Ok(ExpressionResult::Present)
    }
}
