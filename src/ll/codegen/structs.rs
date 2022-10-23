//! Code generation for structs and struct-related things.

use std::{collections::HashMap, rc::Rc};

use super::{
    variables::{VariableAllocation, VariablePlace},
    CodeGenerator, ExpressionResult,
};
use crate::ll::{
    ast::{Ast, NodeId},
    bytecode::{Opcode, Opr24},
    common::{Error, ErrorKind},
};

#[derive(Debug, Default)]
pub(super) struct StructData {
    /// Mapping from field names to indices.
    pub fields: HashMap<Rc<str>, Opr24>,
    /// The `self` variable. Used in `@field` lookups.
    pub receiver: Option<VariablePlace>,
}

impl StructData {
    /// Returns the index of the field with the given name, or creates that field if it doesn't
    /// exist yet.
    pub(super) fn get_or_create_field(&mut self, name: &str) -> Result<Opr24, ErrorKind> {
        if !self.fields.contains_key(name) {
            let index = Opr24::try_from(self.fields.len()).map_err(|_| ErrorKind::TooManyFields)?;
            self.fields.insert(Rc::from(name), index);
            Ok(index)
        } else {
            Ok(*self.fields.get(name).unwrap())
        }
    }

    /// Returns the index of the field with the given name, or `None` if there is no such field.
    pub(super) fn get_field(&self, name: &str) -> Option<Opr24> {
        self.fields.get(name).copied()
    }
}

impl<'e> CodeGenerator<'e> {
    /// Generates code for a field lookup.
    pub(super) fn generate_field(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        let (name, _) = ast.node_pair(node);
        let name = ast.string(name).unwrap();
        let struct_data = self
            .struct_data
            .as_deref()
            .ok_or_else(|| ast.error(node, ErrorKind::FieldOutsideOfImpl))?;
        let field_id = struct_data
            .get_field(name)
            .ok_or_else(|| ast.error(node, ErrorKind::FieldDoesNotExist(Rc::clone(name))))?;
        // Unwrapping is OK here because fields are not allowed outside of functions, and each
        // function with `StructData` passed in assigns to `receiver` at the start of its
        // code generation.
        let receiver = struct_data.receiver.unwrap();
        self.generate_variable_load(receiver);
        self.chunk.emit((Opcode::GetField, field_id));
        Ok(ExpressionResult::Present)
    }

    /// Generates code for a struct declaration.
    pub(super) fn generate_struct(
        &mut self,
        ast: &Ast,
        node: NodeId,
    ) -> Result<ExpressionResult, Error> {
        let (name, _) = ast.node_pair(node);
        let name = ast.string(name).unwrap();

        self.chunk.emit(Opcode::CreateType);
        self.chunk.emit_string(name);
        let variable = self
            .create_variable(name, VariableAllocation::Allocate)
            .map_err(|kind| ast.error(node, kind))?;
        self.generate_variable_assign(variable);

        Ok(ExpressionResult::Present)
    }
}
