//! Code generation for tuples and records, which are closely related.
//! Tuples and records are both essentially just syntax sugar for anonymous structs.

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId},
        bytecode::{Opcode, Opr24},
        error::LanguageErrorKind,
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
        self.chunk.emit((
            Opcode::CreateTuple,
            Opr24::try_from(elements.len())
                .map_err(|_| ast.error(node, LanguageErrorKind::TupleHasTooManyElements))?,
        ));
        self.library.generate_tuple(self.env, self.gc, elements.len());
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
