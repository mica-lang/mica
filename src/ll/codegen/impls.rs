//! Code generation for `impl` blocks.

use std::rc::Rc;

use super::{
    functions::{FunctionCallConv, GenerateFunctionOptions},
    structs::StructData,
    CodeGenerator, Expression, ExpressionResult,
};
use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    bytecode::{ImplementedTraitIndex, MethodSignature, Opcode, Prototype},
    common::{Error, ErrorKind, RenderedSignature},
};

/// The state of generating an `impl` block.
struct ImplGenerationState<'p> {
    proto: &'p mut Prototype,
    has_constructor: bool,
    implemented_trait_index: Option<ImplementedTraitIndex>,
}

impl<'e> CodeGenerator<'e> {
    /// Generates code for an `impl` block.
    pub(super) fn generate_impl(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        let (implementee, _) = ast.node_pair(node);
        let items = ast.children(node).unwrap();

        // Push the implementee onto the stack first. Implemented traits follow.
        self.generate_node(ast, implementee, Expression::Used)?;

        // There's no need to save any old struct data because `impl` blocks don't nest freely.
        // Yes, you can create an `impl` block in a function inside this `impl` block, but that
        // function is handled by a different generator.
        self.struct_data = Some(Box::new(StructData::default()));

        let mut proto = Prototype::default();
        let mut state = ImplGenerationState {
            proto: &mut proto,
            has_constructor: false,
            implemented_trait_index: None,
        };
        for &node in items {
            self.generate_impl_item(ast, node, &mut state, true)?;
        }

        let proto_id = self.env.create_prototype(proto).map_err(|kind| ast.error(node, kind))?;
        self.chunk.emit((Opcode::Implement, proto_id.to_opr24()));

        self.struct_data = None;

        Ok(ExpressionResult::Present)
    }

    /// Generates code for a single item in an `impl` block.
    fn generate_impl_item(
        &mut self,
        ast: &Ast,
        node: NodeId,
        state: &mut ImplGenerationState,
        allow_as: bool,
    ) -> Result<(), Error> {
        match ast.kind(node) {
            NodeKind::Func => {
                let (head, _) = ast.node_pair(node);
                let (name_node, parameters) = ast.node_pair(head);
                if ast.kind(name_node) != NodeKind::Identifier {
                    return Err(ast.error(node, ErrorKind::MissingMethodName));
                }
                let name = Rc::clone(ast.string(name_node).unwrap());
                let (kind, _) = ast.node_pair(parameters);

                let call_conv = match ast.kind(kind) {
                    NodeKind::Empty => FunctionCallConv::Instance,
                    NodeKind::Static => FunctionCallConv::Static,
                    NodeKind::Constructor => {
                        let allow_new_fields = !state.has_constructor;
                        state.has_constructor = true;
                        FunctionCallConv::Constructor { allow_new_fields }
                    }
                    _ => unreachable!(),
                };
                let function = self.generate_function(
                    ast,
                    node,
                    GenerateFunctionOptions { name: Rc::clone(&name), call_conv },
                )?;

                if let Some(trait_index) = state.implemented_trait_index {
                    // For trait instance methods, method ID resolution is performed at runtime.
                    let key = (Rc::clone(&name), 1 + function.parameter_count, trait_index);
                    if state.proto.trait_instance.insert(key, function.id).is_some() {
                        return Err(ast.error(
                            name_node,
                            ErrorKind::MethodAlreadyImplemented(RenderedSignature {
                                name,
                                arity: Some(function.parameter_count),
                                // Note that we do not have a way of knowing the actual name of the
                                // trait, so we simply don't display it.
                                trait_name: None,
                            }),
                        ));
                    }
                } else {
                    // For regular instance methods, method ID resolution is performed during
                    // compilation.
                    let signature = MethodSignature::new(name, 1 + function.parameter_count);
                    let method_id = self
                        .env
                        .get_or_create_method_index(&signature)
                        .map_err(|kind| ast.error(node, kind))?;

                    let map = match ast.kind(kind) {
                        NodeKind::Empty => &mut state.proto.instance,
                        NodeKind::Static | NodeKind::Constructor => &mut state.proto.statics,
                        _ => unreachable!(),
                    };
                    if map.insert(method_id, function.id).is_some() {
                        return Err(ast.error(
                            name_node,
                            ErrorKind::MethodAlreadyImplemented(RenderedSignature {
                                name: signature.name,
                                arity: Some(function.parameter_count),
                                trait_name: None,
                            }),
                        ));
                    }
                }
            }

            NodeKind::ImplAs => {
                if !allow_as {
                    return Err(ast.error(node, ErrorKind::AsCannotNest));
                }
                let (trait_expr, _) = ast.node_pair(node);
                let as_items = ast.children(node).unwrap();
                self.generate_node(ast, trait_expr, Expression::Used)?;
                let trait_index =
                    state.proto.implement_next_trait().map_err(|e| ast.error(node, e))?;
                let mut state = ImplGenerationState {
                    proto: state.proto,
                    implemented_trait_index: Some(trait_index),
                    ..*state
                };
                for &item in as_items {
                    self.generate_impl_item(ast, item, &mut state, false)?;
                }
            }

            // NB: If other item types are allowed, don't forget to change the error message for
            // InvalidImplItem.
            _ => return Err(ast.error(node, ErrorKind::InvalidImplItem)),
        }

        Ok(())
    }
}
