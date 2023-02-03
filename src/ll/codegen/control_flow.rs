//! Control flow expressions.

use super::{variables::VariableAllocation, CodeGenerator, Expression, ExpressionResult};
use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    bytecode::{Opcode, Opr24},
    error::{LanguageError, LanguageErrorKind},
};

#[derive(Debug, Default)]
pub(super) struct BreakableBlock {
    /// A list of offsets where `breaks` should be backpatched.
    pub(super) breaks: Vec<usize>,
    start: usize,
}

impl<'e> CodeGenerator<'e> {
    /// Pushes a new breakable block.
    pub(super) fn push_breakable_block(&mut self) {
        let start = self.chunk.emit(Opcode::Nop);
        self.breakable_blocks.push(BreakableBlock { breaks: Vec::new(), start });
    }

    /// Pops the topmost breakable block.
    pub(super) fn pop_breakable_block(&mut self) {
        let block = self.breakable_blocks.pop().unwrap();
        if !block.breaks.is_empty() {
            self.chunk.patch(block.start, Opcode::EnterBreakableBlock);
            for jump in block.breaks {
                // Unwrapping is safe here because if the loop is too large the error was caught
                // already before `pop_breakable_block` was called.
                self.chunk.patch(jump, Opcode::jump_forward(jump, self.chunk.len()).unwrap());
            }
            self.chunk.emit((Opcode::ExitBreakableBlock, 1));
        }
    }
}

impl<'e> CodeGenerator<'e> {
    /// Generates code for a `do..end` expression.
    pub(super) fn generate_do(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let children = ast.children(node).unwrap();
        self.push_scope();
        self.generate_node_list(ast, children)?;
        self.pop_scope();
        Ok(ExpressionResult::Present)
    }

    /// Generates code for an `if..elif..else..end` expression.
    pub(super) fn generate_if(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let branches = ast.children(node).unwrap();
        let mut jumps_to_end = Vec::new();

        for (i, &branch) in branches.iter().enumerate() {
            // We need to discard the previous branch's condition (if there was a previous branch).
            if i > 0 {
                self.chunk.emit(Opcode::Discard);
            }

            let then = ast.children(branch).unwrap();
            match ast.kind(branch) {
                NodeKind::IfBranch => {
                    // Generate the condition.
                    let (condition, _) = ast.node_pair(branch);
                    self.push_scope();
                    self.generate_node(ast, condition, Expression::Used)?;
                    // Generate a Nop that is later backpatched with a ConditionalJumpForward.
                    let jump = self.chunk.emit(Opcode::Nop);
                    self.chunk.emit(Opcode::Discard); // The condition has to be discarded.
                    self.generate_node_list(ast, then)?;
                    self.pop_scope();
                    let jump_to_end = self.chunk.emit(Opcode::Nop);
                    jumps_to_end.push(jump_to_end);
                    self.chunk.patch(
                        jump,
                        Opcode::jump_forward_if_falsy(jump, self.chunk.len())
                            .map_err(|_| ast.error(branch, LanguageErrorKind::IfBranchTooLarge))?,
                    );
                }

                NodeKind::ElseBranch => {
                    self.push_scope();
                    self.generate_node_list(ast, then)?;
                    self.pop_scope();
                }

                _ => unreachable!(),
            }
        }

        // If there was no `else` branch, we need to patch in an implicit one that returns `nil`.
        if ast.kind(*branches.last().unwrap()) != NodeKind::ElseBranch {
            self.chunk.emit(Opcode::Discard);
            self.chunk.emit(Opcode::PushNil);
        }

        // Backpatch all jumps to end with an unconditional jump forward.
        for jump in jumps_to_end {
            self.chunk.patch(
                jump,
                Opcode::jump_forward(jump, self.chunk.len())
                    .map_err(|_| ast.error(node, LanguageErrorKind::IfExpressionTooLarge))?,
            );
        }

        Ok(ExpressionResult::Present)
    }

    /// Generates code for an `and` infix operator.
    pub(super) fn generate_and(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (left, right) = ast.node_pair(node);
        self.push_scope();
        self.generate_node(ast, left, Expression::Used)?;
        let jump_past_right = self.chunk.emit(Opcode::Nop);
        self.chunk.emit(Opcode::Discard);
        self.generate_node(ast, right, Expression::Used)?;
        self.chunk.patch(
            jump_past_right,
            Opcode::jump_forward_if_falsy(jump_past_right, self.chunk.len())
                .map_err(|_| ast.error(node, LanguageErrorKind::OperatorRhsTooLarge))?,
        );
        self.pop_scope();
        Ok(ExpressionResult::Present)
    }

    /// Generates code for an `or` infix operator.
    pub(super) fn generate_or(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (left, right) = ast.node_pair(node);
        self.push_scope();
        self.generate_node(ast, left, Expression::Used)?;
        let jump_past_right = self.chunk.emit(Opcode::Nop);
        self.chunk.emit(Opcode::Discard);
        self.generate_node(ast, right, Expression::Used)?;
        self.chunk.patch(
            jump_past_right,
            Opcode::jump_forward_if_truthy(jump_past_right, self.chunk.len())
                .map_err(|_| ast.error(node, LanguageErrorKind::OperatorRhsTooLarge))?,
        );
        self.pop_scope();
        Ok(ExpressionResult::Present)
    }

    pub(super) fn generate_conditional_loop(
        &mut self,
        ast: &Ast,
        node: NodeId,
        generate_condition: &dyn Fn(&mut CodeGenerator<'_>) -> Result<(), LanguageError>,
        generate_body: &dyn Fn(&mut CodeGenerator<'_>) -> Result<(), LanguageError>,
    ) -> Result<(), LanguageError> {
        // The outer scope, so that variables can be declared in the condition.
        self.push_scope();
        // The breakable block.
        self.push_breakable_block();

        let start = self.chunk.len();
        generate_condition(self)?;
        let jump_to_end = self.chunk.emit(Opcode::Nop);
        // Discard the condition if it's true.
        self.chunk.emit(Opcode::Discard);

        generate_body(self)?;
        // While loops don't yield a value.
        self.chunk.emit(Opcode::Discard);

        self.chunk.emit(
            Opcode::jump_backward(self.chunk.len(), start)
                .map_err(|_| ast.error(node, LanguageErrorKind::LoopTooLarge))?,
        );
        self.chunk.patch(
            jump_to_end,
            Opcode::jump_forward_if_falsy(jump_to_end, self.chunk.len())
                .map_err(|_| ast.error(node, LanguageErrorKind::LoopTooLarge))?,
        );
        // Discard the condition if it's false.
        self.chunk.emit(Opcode::Discard);

        // Because loops are expressions, they must produce a value. That value is `nil`.
        self.chunk.emit(Opcode::PushNil);

        // `break`s produce a value (or `nil` by default), so we need to jump over the
        // fallback `PushNil`.
        self.pop_breakable_block();
        self.pop_scope();

        Ok(())
    }

    /// Generates code for a `while..do..end` loop.
    pub(super) fn generate_while(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (condition, _) = ast.node_pair(node);
        let body = ast.children(node).unwrap();

        self.generate_conditional_loop(
            ast,
            node,
            &|generator| generator.generate_node(ast, condition, Expression::Used),
            &|generator| generator.generate_node_list(ast, body),
        )?;

        Ok(ExpressionResult::Present)
    }

    /// Generates code for a `for` loop.
    pub(super) fn generate_for(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (binding, iterator) = ast.node_pair(node);
        let body = ast.children(node).unwrap();

        self.push_scope();

        let iterator_var = self
            .create_variable("<iterator>", VariableAllocation::Allocate)
            .map_err(|kind| ast.error(iterator, kind))?;
        self.generate_node(ast, iterator, Expression::Used)?;
        self.generate_variable_sink(iterator_var);

        self.generate_conditional_loop(
            ast,
            node,
            &|generator| {
                generator.generate_variable_load(iterator_var);
                generator.chunk.emit((
                    Opcode::CallMethod,
                    Opr24::pack((generator.library.builtin_traits.iterator_has_next.to_u16(), 1)),
                ));
                Ok(())
            },
            &|generator| {
                generator.generate_variable_load(iterator_var);
                generator.chunk.emit((
                    Opcode::CallMethod,
                    Opr24::pack((generator.library.builtin_traits.iterator_next.to_u16(), 1)),
                ));
                generator.generate_pattern_destructuring(ast, binding, Expression::Discarded)?;
                generator.generate_node_list(ast, body)?;
                Ok(())
            },
        )?;

        self.pop_scope();

        Ok(ExpressionResult::Present)
    }

    /// Generates a `break` expression.
    pub(super) fn generate_break(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (right, _) = ast.node_pair(node);
        if right != NodeId::EMPTY {
            self.generate_node(ast, right, Expression::Used)?;
        } else {
            let _ = self.generate_nil();
        }
        let jump = self.chunk.emit(Opcode::Nop);
        if let Some(block) = self.breakable_blocks.last_mut() {
            block.breaks.push(jump);
        } else {
            return Err(ast.error(node, LanguageErrorKind::BreakOutsideOfLoop));
        }
        Ok(ExpressionResult::NoReturn)
    }

    /// Generates code for a `return` expression.
    pub(super) fn generate_return(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (value, _) = ast.node_pair(node);
        // Unlike breaking out of a loop, returning is super simple, as it implies that all locals
        // are taken off the stack.
        if value == NodeId::EMPTY {
            let _ = self.generate_nil();
        } else {
            self.generate_node(ast, value, Expression::Used)?;
        }
        self.chunk.emit(Opcode::Return);
        Ok(ExpressionResult::NoReturn)
    }
}
