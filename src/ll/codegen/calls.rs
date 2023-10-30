//! Code generation for various types of function calls.

use std::rc::Rc;

use super::{CodeGenerator, Expression, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId, NodeKind},
        bytecode::{MethodSignature, Opcode, Opr24},
        error::{LanguageError, LanguageErrorKind},
    },
    MethodParameterCount,
};

impl<'e> CodeGenerator<'e> {
    /// Generates code for a function call.
    pub(super) fn generate_call(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (function, _) = ast.node_pair(node);
        match ast.kind(function) {
            // Method calls need special treatment.
            NodeKind::Dot => {
                let (receiver, name) = ast.node_pair(function);
                if ast.kind(name) != NodeKind::Identifier {
                    return Err(ast.error(name, LanguageErrorKind::InvalidMethodName));
                }
                let name = ast.string(name).unwrap();

                // Generate the receiver and arguments.
                self.generate_node(ast, receiver, Expression::Used)?;
                let arguments = ast.children(node).unwrap();
                for &argument in arguments {
                    self.generate_node(ast, argument, Expression::Used)?;
                }

                // Construct the call.
                let parameter_count =
                    MethodParameterCount::from_count_without_self(arguments.len())
                        .map_err(|_| ast.error(node, LanguageErrorKind::TooManyArguments))?;
                let signature = MethodSignature::new(Rc::clone(name), parameter_count);
                let method_index = self
                    .env
                    .get_or_create_method_index(&signature)
                    .map_err(|kind| ast.error(node, kind))?;
                self.chunk.emit((
                    Opcode::CallMethod,
                    Opr24::pack((method_index.to_u16(), parameter_count.to_count_with_self())),
                ));
            }
            _ => {
                self.generate_node(ast, function, Expression::Used)?;
                let arguments = ast.children(node).unwrap();
                for &argument in arguments {
                    self.generate_node(ast, argument, Expression::Used)?;
                }
                self.chunk.emit((
                    Opcode::Call,
                    Opr24::try_from(arguments.len())
                        .map_err(|_| ast.error(node, LanguageErrorKind::TooManyArguments))?,
                ));
            }
        }
        Ok(ExpressionResult::Present)
    }

    /// Generates code for a bare dot call (without parentheses).
    pub(super) fn generate_dot(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (left, method) = ast.node_pair(node);
        self.generate_node(ast, left, Expression::Used)?;

        if ast.kind(method) != NodeKind::Identifier {
            return Err(ast.error(method, LanguageErrorKind::InvalidMethodName));
        }
        let signature = MethodSignature::new(
            Rc::clone(ast.string(method).unwrap()),
            MethodParameterCount::from_count_with_self(1),
        );
        let method_index = self
            .env
            .get_or_create_method_index(&signature)
            .map_err(|kind| ast.error(node, kind))?;
        self.chunk
            .emit((Opcode::CallMethod, Opr24::pack((method_index.to_u16(), 1))));

        Ok(ExpressionResult::Present)
    }
}
