//! Code generation for tuples and records, which are closely related.
//! Tuples and records are both essentially just syntax sugar for anonymous structs.

use std::{ops::Deref, rc::Rc};

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId, NodeKind},
        bytecode::{make_record_identifier, MethodSignature, Opcode, Opr24},
        error::LanguageErrorKind,
    },
    LanguageError, MethodParameterCount,
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

        let identifier = make_record_identifier(fields.iter().map(|&rc| rc.deref()));
        let record_type_index = self
            .library
            .get_or_generate_record(self.env, self.gc, &identifier)
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

    pub(super) fn generate_record_destructuring(
        &mut self,
        ast: &Ast,
        node: NodeId,
        result: Expression,
    ) -> Result<(), LanguageError> {
        let pairs = ast.children(node).unwrap();
        let is_non_exhaustive =
            !pairs.is_empty() && ast.kind(*pairs.last().unwrap()) == NodeKind::Rest;

        fn destructure_member(
            generator: &mut CodeGenerator,
            ast: &Ast,
            key: NodeId,
            value: NodeId,
        ) -> Result<(), LanguageError> {
            let pattern = if value == NodeId::EMPTY { key } else { value };
            if ast.kind(pattern) == NodeKind::Underscore {
                generator.chunk.emit(Opcode::Discard);
                Ok(())
            } else {
                generator.generate_pattern_destructuring(ast, pattern, Expression::Discarded)
            }
        }

        if is_non_exhaustive {
            self.chunk.emit(Opcode::DestructureRecordNonExhaustive);

            // Skip the last element, which is guaranteed to be Rest.
            for &pair in pairs.iter().rev().skip(1) {
                let (key, value) = ast.node_pair(pair);

                let signature = MethodSignature::new(
                    Rc::clone(ast.string(key).unwrap()),
                    MethodParameterCount::from_count_with_self(1),
                );
                let method_index = self
                    .env
                    .get_or_create_method_index(&signature)
                    .map_err(|e| ast.error(key, e))?;
                // For now this simply calls methods on the record; this is the simplest way of
                // doing things without complicating the implementation too much.
                // We can swap it out for something more performant if it becomes too slow.
                self.chunk.emit(Opcode::Duplicate);
                self.chunk.emit((Opcode::CallMethod, Opr24::pack((method_index.to_u16(), 1))));

                destructure_member(self, ast, key, value)?;
            }
        } else {
            let identifier = make_record_identifier(pairs.iter().map(|&pair| {
                let (key, _) = ast.node_pair(pair);
                ast.string(key).unwrap().deref()
            }));
            let record_type_index = self
                .library
                .get_or_generate_record(self.env, self.gc, &identifier)
                .map_err(|_| ast.error(node, LanguageErrorKind::TooManyRecords))?;
            // In case the record is used after destructuring, we need to copy it so that the
            // original stays on the stack.
            if result == Expression::Used {
                self.chunk.emit(Opcode::Duplicate);
            }
            self.chunk.emit((Opcode::DestructureRecord, record_type_index.to_opr24()));
            for &pair in pairs.iter().rev() {
                let (key, value) = ast.node_pair(pair);
                destructure_member(self, ast, key, value)?;
            }
        }

        Ok(())
    }
}
