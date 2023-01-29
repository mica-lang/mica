//! Code generation for assignment and pattern matching.

use std::rc::Rc;

use super::{variables::VariableAllocation, CodeGenerator, Expression, ExpressionResult};
use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    bytecode::Opcode,
    error::{LanguageError, LanguageErrorKind},
};

impl<'e> CodeGenerator<'e> {
    /// Generates code for destructuring a pattern.
    ///
    /// The generated code assumes there's a value to destructure at the top of the stack (called
    /// the *scrutinee*.)
    pub(super) fn generate_pattern_destructuring(
        &mut self,
        ast: &Ast,
        node: NodeId,
        result: Expression,
    ) -> Result<(), LanguageError> {
        match ast.kind(node) {
            NodeKind::Identifier => {
                let name = ast.string(node).unwrap();
                let variable = self
                    .create_variable(name, VariableAllocation::Allocate)
                    .map_err(|kind| ast.error(node, kind))?;
                match result {
                    Expression::Used => self.generate_variable_assign(variable),
                    Expression::Discarded => self.generate_variable_sink(variable),
                }
            }
            _ => return Err(ast.error(node, LanguageErrorKind::InvalidPattern)),
        }
        Ok(())
    }

    /// Generates code for a `let` expression.
    pub(super) fn generate_let(
        &mut self,
        ast: &Ast,
        node: NodeId,
        result: Expression,
    ) -> Result<ExpressionResult, LanguageError> {
        let (assignment, _) = ast.node_pair(node);
        if ast.kind(assignment) != NodeKind::Assign {
            return Err(ast.error(assignment, LanguageErrorKind::LetRhsMustBeAssignment));
        }
        let (place, value) = ast.node_pair(assignment);
        self.generate_node(ast, value, Expression::Used)?;
        self.generate_pattern_destructuring(ast, place, result)?;
        Ok(match result {
            Expression::Discarded => ExpressionResult::Absent,
            Expression::Used => ExpressionResult::Present,
        })
    }

    /// Generates code for an assignment.
    pub(super) fn generate_assignment(
        &mut self,
        ast: &Ast,
        node: NodeId,
        result: Expression,
    ) -> Result<ExpressionResult, LanguageError> {
        let (target, value) = ast.node_pair(node);
        self.generate_node(ast, value, Expression::Used)?;

        match ast.kind(target) {
            NodeKind::Identifier => {
                let name = ast.string(target).unwrap();
                let variable = if let Some(slot) =
                    self.lookup_variable(name).map_err(|kind| ast.error(target, kind))?
                {
                    slot
                } else {
                    return Err(
                        ast.error(target, LanguageErrorKind::VariableDoesNotExist(Rc::clone(name)))
                    );
                };
                match result {
                    Expression::Used => self.generate_variable_assign(variable),
                    Expression::Discarded => self.generate_variable_sink(variable),
                }
            }
            NodeKind::Field => {
                let (name, _) = ast.node_pair(target);
                let name = ast.string(name).unwrap();
                if let Some(struct_data) = self.struct_data.as_mut() {
                    let field = if self.allow_new_fields {
                        struct_data
                            .get_or_create_field(name)
                            .map_err(|kind| ast.error(node, kind))?
                    } else {
                        struct_data.get_field(name).ok_or_else(|| {
                            ast.error(target, LanguageErrorKind::FieldDoesNotExist(Rc::clone(name)))
                        })?
                    };
                    // Unwrapping is OK here because `receiver` is assigned at the start of each
                    // function in an `impl` block.
                    let receiver = struct_data.receiver.unwrap();
                    self.generate_variable_load(receiver);
                    self.chunk.emit(match result {
                        Expression::Used => (Opcode::AssignField, field),
                        Expression::Discarded => (Opcode::SinkField, field),
                    });
                } else {
                    return Err(ast.error(target, LanguageErrorKind::FieldOutsideOfImpl));
                }
                // In constructors, we need to keep track of which fields were assigned to report
                // errors about missing field values.
                if self.is_constructor && !self.allow_new_fields {
                    self.assigned_fields.insert(Rc::clone(name));
                }
            }
            _ => return Err(ast.error(target, LanguageErrorKind::InvalidAssignment)),
        }

        Ok(match result {
            Expression::Discarded => ExpressionResult::Absent,
            Expression::Used => ExpressionResult::Present,
        })
    }
}
