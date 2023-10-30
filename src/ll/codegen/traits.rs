//! Code generation for traits. Includes public API for building new traits programatically.

use std::{collections::HashSet, fmt, rc::Rc};

use super::{variables::VariableAllocation, CodeGenerator, ExpressionResult};
use crate::{
    ll::{
        ast::{Ast, NodeId, NodeKind},
        bytecode::{
            Chunk, Environment, Function, FunctionIndex, FunctionKind, MethodIndex,
            MethodSignature, Opcode, Opr24, TraitIndex,
        },
        error::{LanguageError, LanguageErrorKind, Location, RenderedSignature},
    },
    FunctionParameterCount, MethodParameterCount,
};

pub struct TraitBuilder<'b> {
    pub env: &'b mut Environment,
    parent_chunk: Option<&'b Chunk>,
    trait_id: TraitIndex,
    required_methods: HashSet<MethodIndex>,
    shims: Vec<(MethodIndex, FunctionIndex)>,
}

impl<'b> TraitBuilder<'b> {
    pub fn new(
        env: &'b mut Environment,
        parent_chunk: Option<&'b Chunk>,
        name: Rc<str>,
    ) -> Result<Self, LanguageErrorKind> {
        let trait_id = env.create_trait(name)?;
        Ok(Self {
            env,
            parent_chunk,
            trait_id,
            required_methods: HashSet::new(),
            shims: vec![],
        })
    }

    /// Generates the *shim* for a trait method. The shim is responsible for calling the actual
    /// method.
    fn generate_method_shim(
        &mut self,
        trait_name: &str,
        method_id: MethodIndex,
        method_signature: &MethodSignature,
    ) -> Result<FunctionIndex, LanguageErrorKind> {
        let parameter_count = method_signature.parameter_count.to_count_with_self();

        let (module_name, codegen_location) = if let Some(parent_chunk) = self.parent_chunk {
            (
                Rc::clone(&parent_chunk.module_name),
                parent_chunk.codegen_location,
            )
        } else {
            (Rc::from("FFI"), Location::UNINIT)
        };

        let mut chunk = Chunk::new(module_name);
        chunk.codegen_location = codegen_location;
        chunk.emit((
            Opcode::CallMethod,
            Opr24::pack((method_id.to_u16(), parameter_count)),
        ));
        chunk.emit(Opcode::Return);

        let shim_name = Rc::from(format!(
            "trait {trait_name}.{} <shim>",
            method_signature.name
        ));
        let chunk = Rc::new(chunk);
        let function_id = self.env.create_function(Function {
            name: shim_name,
            parameter_count: FunctionParameterCount::Fixed(
                method_signature.parameter_count.to_count_without_self() as u16,
            ),
            kind: FunctionKind::Bytecode {
                chunk,
                captured_locals: vec![],
            },
            hidden_in_stack_traces: true,
        })?;

        Ok(function_id)
    }

    /// Adds a function into the trait and returns its method ID and function ID.
    ///
    /// The parameter count does not include the `self` parameter.
    pub fn add_method(
        &mut self,
        name: Rc<str>,
        parameter_count: MethodParameterCount,
    ) -> Result<MethodIndex, LanguageErrorKind> {
        let trait_name = Rc::clone(&self.env.get_trait(self.trait_id).unwrap().name);
        let signature = MethodSignature {
            name,
            parameter_count,
            trait_id: Some(self.trait_id),
        };
        let method_id = self.env.get_or_create_method_index(&signature)?;

        let parameter_count_with_receiving_trait = MethodParameterCount::from_count_with_self(
            parameter_count
                .to_count_with_self()
                .checked_add(1)
                .ok_or(LanguageErrorKind::TooManyParameters)?,
        );
        let shim_signature = MethodSignature {
            parameter_count: parameter_count_with_receiving_trait,
            trait_id: None,
            ..signature.clone()
        };
        let shim_method_id = self.env.get_or_create_method_index(&shim_signature)?;
        let shim_function_id = self.generate_method_shim(&trait_name, method_id, &signature)?;
        self.shims.push((shim_method_id, shim_function_id));

        if !self.required_methods.insert(method_id) {
            return Err(LanguageErrorKind::TraitAlreadyHasMethod(
                RenderedSignature {
                    name: signature.name,
                    parameter_count: signature.parameter_count.to_count_without_self(),
                    // Do not duplicate the trait name in the error message.
                    trait_name: None,
                },
            ));
        }

        Ok(method_id)
    }

    /// Finishes building the trait, returns its trait ID, and gives back the mutable reference to
    /// the environment.
    pub fn build(self) -> (TraitIndex, &'b mut Environment) {
        let prototype = self.env.get_trait_mut(self.trait_id).unwrap();
        prototype.required = self.required_methods;
        prototype.shims = self.shims;
        (self.trait_id, self.env)
    }
}

impl<'e> fmt::Debug for TraitBuilder<'e> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TraitBuilder").finish_non_exhaustive()
    }
}

impl<'e> CodeGenerator<'e> {
    /// Generates code for a trait definition.
    pub(super) fn generate_trait(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, LanguageError> {
        let (trait_name, _) = ast.node_pair(node);
        let trait_name = ast.string(trait_name).unwrap();
        let items = ast.children(node).unwrap();

        let mut builder = TraitBuilder::new(self.env, Some(&self.chunk), Rc::clone(trait_name))
            .map_err(|e| ast.error(node, e))?;

        for &item in items {
            match ast.kind(item) {
                NodeKind::Func => {
                    let (head, body) = ast.node_pair(item);
                    let (name, params) = ast.node_pair(head);
                    if body != NodeId::EMPTY {
                        return Err(ast.error(body, LanguageErrorKind::TraitMethodCannotHaveBody));
                    }
                    if name == NodeId::EMPTY {
                        return Err(ast.error(head, LanguageErrorKind::MissingMethodName));
                    }
                    if ast.node_pair(params).0 != NodeId::EMPTY {
                        return Err(ast.error(head, LanguageErrorKind::FunctionKindInTrait));
                    }

                    let _method_id = builder
                        .add_method(
                            Rc::clone(ast.string(name).unwrap()),
                            MethodParameterCount::from_count_without_self(ast.len(params).unwrap())
                                .map_err(|_| {
                                    ast.error(params, LanguageErrorKind::TooManyParameters)
                                })?,
                        )
                        .map_err(|e| ast.error(item, e))?;
                }
                _ => return Err(ast.error(item, LanguageErrorKind::InvalidTraitItem)),
            }
        }

        let (trait_id, _) = builder.build();

        self.chunk.emit((Opcode::CreateTrait, trait_id.to_opr24()));
        let variable = self
            .create_variable(trait_name, VariableAllocation::Allocate)
            .map_err(|k| ast.error(node, k))?;
        self.generate_variable_assign(variable);

        Ok(ExpressionResult::Present)
    }
}
