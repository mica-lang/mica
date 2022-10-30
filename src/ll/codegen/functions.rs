//! Code generation for functions.

use std::{mem, rc::Rc};

use super::{variables::VariableAllocation, CodeGenerator, Expression, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId},
        bytecode::{Function, FunctionIndex, FunctionKind, Opcode, Opr24},
        error::{Error, ErrorKind},
    },
    FunctionParameterCount,
};

/// The calling convention of a function.
#[derive(Debug, Clone, Copy)]
pub(super) enum FunctionCallConv {
    /// A bare function: `self` is invisible, fields cannot be assigned.
    Bare,
    /// A static function: `self` is visible, fields cannot be assigned.
    Static,
    /// An instance function: `self` is visible, fields can be assigned but not created.
    Instance,
    /// A constructor: `self` is created from scratch, fields can created if `allow_new_fields` is
    /// true.
    Constructor { allow_new_fields: bool },
}

impl FunctionCallConv {
    fn has_field_access(&self) -> bool {
        matches!(self, Self::Instance | Self::Constructor { .. })
    }

    fn has_visible_self(&self) -> bool {
        // Note that `Constructor` does not have a visible `self`, because it's an allocated local
        // rather than inherited.
        matches!(self, Self::Static | Self::Instance)
    }

    fn is_constructor(&self) -> bool {
        matches!(self, Self::Constructor { .. })
    }

    fn allow_new_fields(&self) -> bool {
        matches!(self, Self::Constructor { allow_new_fields: true })
    }
}

pub(super) struct GenerateFunctionOptions {
    pub(crate) name: Rc<str>,
    pub(crate) call_conv: FunctionCallConv,
}

pub(super) struct GeneratedFunction {
    pub(crate) id: FunctionIndex,

    /// The number of **explicit** parameters the function has.
    ///
    /// This means that if the function was generated with a non-bare calling convention, this
    /// number **does not** include the `self` parameter.
    pub(crate) parameter_count: u16,
}

impl<'e> CodeGenerator<'e> {
    pub(super) fn generate_function(
        &mut self,
        ast: &Ast,
        node: NodeId,
        GenerateFunctionOptions { name, call_conv }: GenerateFunctionOptions,
    ) -> Result<GeneratedFunction, Error> {
        let (head, body) = ast.node_pair(node);
        let (_, parameters) = ast.node_pair(head);
        let parameter_list = ast.children(parameters).unwrap();

        let mut generator =
            CodeGenerator::new(Rc::clone(&self.chunk.module_name), self.env, self.builtin_traits);
        // NOTE: Hopefully the allocation from this mem::take gets optimized out.
        generator.locals.parent = Some(mem::take(&mut self.locals));
        if call_conv.has_field_access() {
            generator.struct_data = self.struct_data.take();
            generator.allow_new_fields = call_conv.allow_new_fields();
        }
        generator.is_constructor = call_conv.is_constructor();

        // Push a scope to start creating local variables.
        generator.push_scope();
        // Create local variables for all the parameters.
        // Note that in bare functions, the function itself acts as the `self` parameter.
        let receiver = if call_conv.has_visible_self() {
            let receiver = generator.create_variable("self", VariableAllocation::Inherit).unwrap();
            if let Some(struct_data) = generator.struct_data.as_deref_mut() {
                struct_data.receiver = Some(receiver);
            }
            receiver
        } else {
            generator.create_variable("<receiver>", VariableAllocation::Inherit).unwrap()
        };
        for &parameter in parameter_list {
            let parameter_name = ast.string(parameter).unwrap();
            generator
                .create_variable(parameter_name, VariableAllocation::Inherit)
                .map_err(|kind| ast.error(parameter, kind))?;
        }

        // In constructors, we have to create `self` explicitly.
        let create_struct = if call_conv.is_constructor() {
            generator.generate_variable_load(receiver);
            let instruction = generator.chunk.emit(Opcode::Nop);
            let receiver = generator
                .create_variable("self", VariableAllocation::Allocate)
                .map_err(|kind| ast.error(node, kind))?;
            generator.generate_variable_assign(receiver);
            generator.chunk.emit(Opcode::Discard);
            generator.struct_data.as_deref_mut().unwrap().receiver = Some(receiver);
            Some(instruction)
        } else {
            None
        };

        // Generate the body.
        generator.generate_node(ast, body, Expression::Used)?;

        // If we're in a constructor we have to backpatch the `CreateStruct` instruction with the
        // number of fields.
        if let Some(create_struct) = create_struct {
            let field_count = generator.struct_data.as_ref().unwrap().fields.len();
            let field_count = Opr24::try_from(field_count)
                .map_err(|_| ast.error(ast.node_pair(parameters).0, ErrorKind::TooManyFields))?;
            generator.chunk.patch(create_struct, (Opcode::CreateStruct, field_count));
            // We also have to discard whatever was at the top of the stack at the moment and
            // return the struct we constructed.
            generator.chunk.emit(Opcode::Discard);
            let receiver = generator.struct_data.as_ref().unwrap().receiver.unwrap();
            generator.generate_variable_load(receiver);
        }
        // If we're in a constructor, we also have to check if all fields were assigned.
        if call_conv.is_constructor() && !call_conv.allow_new_fields() {
            let struct_data = generator.struct_data.as_ref().unwrap();
            let missing: Vec<_> = struct_data
                .fields
                .keys()
                .filter(|field| !generator.assigned_fields.contains(*field))
                .cloned()
                .collect();
            if !missing.is_empty() {
                return Err(ast.error(node, ErrorKind::MissingFields(missing)));
            }
        }

        // Finish generating the chunk by inserting a `Return` opcode.
        generator.pop_scope();
        generator.chunk.emit(Opcode::Return);

        // Take back what was taken from the parent generator.
        self.locals = generator.locals.parent.take().unwrap();
        if call_conv.has_field_access() {
            self.struct_data = generator.struct_data;
        }

        // Construct the function.
        let parameter_count = u16::try_from(parameter_list.len())
            .map_err(|_| ast.error(parameters, ErrorKind::TooManyParameters))?;
        let function = Function {
            name,
            parameter_count: FunctionParameterCount::Fixed(parameter_count),
            kind: FunctionKind::Bytecode {
                chunk: Rc::new(generator.chunk),
                captured_locals: generator.locals.captures,
            },
            hidden_in_stack_traces: false,
        };
        let function_id =
            self.env.create_function(function).map_err(|kind| ast.error(node, kind))?;

        Ok(GeneratedFunction { id: function_id, parameter_count })
    }

    /// Ensures that a `Func` node is a valid bare function - without a kind, and with a body.
    fn ensure_valid_bare_function(ast: &Ast, node: NodeId) -> Result<(), Error> {
        let (head, body) = ast.node_pair(node);
        let (_name, parameters) = ast.node_pair(head);

        let function_kind = ast.node_pair(parameters).0;
        if function_kind != NodeId::EMPTY {
            return Err(ast.error(function_kind, ErrorKind::FunctionKindOutsideImpl));
        }
        if body == NodeId::EMPTY {
            return Err(ast.error(node, ErrorKind::MissingFunctionBody));
        }
        Ok(())
    }

    /// Generates code for a bare function declaration.
    pub(super) fn generate_function_declaration(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        Self::ensure_valid_bare_function(ast, node)?;

        let (head, _) = ast.node_pair(node);
        let (name_node, _) = ast.node_pair(head);
        let name = ast.string(name_node).unwrap();

        // Create the variable before compiling the function, to allow for recursion.
        let variable = self
            .create_variable(name, VariableAllocation::Allocate)
            .map_err(|kind| ast.error(name_node, kind))?;

        let function = self.generate_function(
            ast,
            node,
            GenerateFunctionOptions { name: Rc::clone(name), call_conv: FunctionCallConv::Bare },
        )?;

        self.chunk.emit((Opcode::CreateClosure, function.id.to_opr24()));
        self.generate_variable_sink(variable);

        Ok(ExpressionResult::Absent)
    }

    /// Generates code for a function expression.
    pub(super) fn generate_function_expression(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        Self::ensure_valid_bare_function(ast, node)?;

        let function = self.generate_function(
            ast,
            node,
            GenerateFunctionOptions {
                name: Rc::from("<anonymous>"),
                call_conv: FunctionCallConv::Bare,
            },
        )?;

        self.chunk.emit((Opcode::CreateClosure, function.id.to_opr24()));
        Ok(ExpressionResult::Present)
    }
}
