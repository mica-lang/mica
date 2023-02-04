//! Code generation for tuples and records, which are closely related.
//! Tuples and records are both essentially just syntax sugar for anonymous structs.

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId, NodeKind},
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
        let pairs = ast.children(node).unwrap();
        // The limit is actually one less than the u32 limit since we reserve 0xFFFF_FFFF as a
        // sentinel to enable context-free bytecode parsing.
        // Someone would really have to be crazy to hit this limit (or any other VM limit, really.)
        if pairs.len() >= u32::MAX as usize - 1 {
            return Err(ast.error(node, LanguageErrorKind::RecordHasTooManyFields));
        }

        let mut fields: Vec<_> =
            pairs.iter().map(|&pair| ast.string(ast.node_pair(pair).0).unwrap()).collect();
        fields.sort();

        let mut field_order = vec![];
        for &pair in pairs {
            let (key, value) = ast.node_pair(pair);
            if ast.kind(key) == NodeKind::Rest {
                return Err(ast.error(key, LanguageErrorKind::RestInRecordConstructor));
            }
            let value = if value == NodeId::EMPTY { key } else { value };
            self.generate_node(ast, value, Expression::Used)?;

            let key = ast.string(key).unwrap();
            let field_index = fields.iter().position(|name| name == &key).unwrap();
            field_order.push(field_index as u32);
        }

        let mut identifier = fields.iter().fold(String::new(), |mut a, b| {
            a.reserve(b.len() + 1);
            a.push_str(b);
            a.push_str("+");
            a
        });
        identifier.pop();
        let record_type_index = self
            .library
            .get_or_generate_record(&mut self.env, &mut self.gc, &identifier)
            .map_err(|_| ast.error(node, LanguageErrorKind::TooManyRecords))?;

        self.chunk.emit((Opcode::CreateRecord, record_type_index.to_opr24()));
        for field_index in field_order {
            self.chunk.emit_u32(field_index);
        }
        // Emit an extra sentinel so that parsing bytecode doesn't require knowing the RecordType
        // to know where the CreateRecord opcode ends.
        self.chunk.emit_u32(0xFFFF_FFFF);

        Ok(ExpressionResult::Present)
    }
}
