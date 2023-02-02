//! Code generation for tuples and records, which are closely related.
//! Tuples and records are both essentially just syntax sugar for anonymous structs.

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId},
        bytecode::Opcode,
    },
    LanguageError,
};

impl<'e> CodeGenerator<'e> {
    pub(super) fn generate_tuple(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let elements = ast.children(node).unwrap();
        for &element in elements {
            self.generate_node(ast, element, Expression::Used)?;
        }
        self.chunk.emit(Opcode::CreateTuple);
        self.env.generate_tuple(elements.len(), self.gc, self.builtin_traits);
        Ok(ExpressionResult::Present)
    }

    pub(super) fn generate_record(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        todo!("records are NYI");
    }
}
