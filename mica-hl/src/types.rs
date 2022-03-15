use std::marker::PhantomData;
use std::rc::Rc;

use mica_language::bytecode::{
   DispatchTable, Environment, Function, FunctionKind, FunctionSignature,
};
use mica_language::value::Closure;

use crate::{ffvariants, Error, ForeignFunction, RawForeignFunction};

/// A descriptor for a dispatch table. Defines which methods are available on the table, as well
/// as their implementations.
#[derive(Default)]
pub(crate) struct DispatchTableDescriptor {
   methods: Vec<(FunctionSignature, RawForeignFunction)>,
}

impl DispatchTableDescriptor {
   /// Builds a dispatch table from this descriptor.
   pub(crate) fn build_dtable(
      self,
      type_name: Rc<str>,
      env: &mut Environment,
   ) -> Result<DispatchTable, Error> {
      let mut dtable = DispatchTable::new(type_name);
      for (signature, f) in self.methods {
         let function_id = env
            .create_function(Function {
               name: Rc::clone(&signature.name),
               parameter_count: signature.arity,
               kind: FunctionKind::Foreign(f),
            })
            .map_err(|_| Error::TooManyFunctions)?;
         let index = env.get_method_index(&signature).map_err(|_| Error::TooManyMethods)?;
         dtable.set_method(
            index,
            Rc::new(Closure {
               function_id,
               captures: Vec::new(),
            }),
         );
      }
      Ok(dtable)
   }
}

/// A builder that allows for binding APIs with user-defined types.
pub struct TypeBuilder<T>
where
   T: ?Sized,
{
   type_name: Rc<str>,
   type_dtable: DispatchTableDescriptor,
   instance_dtable: DispatchTableDescriptor,
   _data: PhantomData<T>,
}

impl<T> TypeBuilder<T>
where
   T: ?Sized,
{
   /// Creates a new `TypeBuilder`.
   pub fn new(type_name: impl Into<Rc<str>>) -> Self {
      let type_name = type_name.into();
      Self {
         type_dtable: Default::default(),
         instance_dtable: Default::default(),
         type_name,
         _data: PhantomData,
      }
   }

   /// Adds a _raw_ instance function to the struct.
   ///
   /// You should generally prefer [`add_function`][`Self::add_function`] instead of this.
   ///
   /// `parameter_count` should reflect the parameter count of the function. Pass `None` if the
   /// function accepts a variable number of arguments. Note that _unlike with bare raw functions_
   /// there can be two functions with the same name defined on a type, as long as they have
   /// different arities. Functions with specific arities take priority over varargs.
   pub fn add_raw_function(
      &mut self,
      name: &str,
      parameter_count: Option<u16>,
      f: RawForeignFunction,
   ) -> &mut Self {
      self.instance_dtable.methods.push((
         FunctionSignature {
            name: Rc::from(name),
            arity: parameter_count,
         },
         f,
      ));
      self
   }

   /// Adds an instance function to the struct.
   pub fn add_function<F, V>(&mut self, name: &str, f: F) -> &mut Self
   where
      V: ffvariants::Method<T>,
      F: ForeignFunction<V>,
   {
      self.add_raw_function(name, f.parameter_count(), f.to_raw_foreign_function())
   }

   /// Destrutcures the struct builder into its type dtable and instance dtable, respectively.
   pub(crate) fn build_dtables(
      self,
      env: &mut Environment,
   ) -> Result<(DispatchTable, DispatchTable), Error> {
      Ok((
         self.type_dtable.build_dtable(Rc::clone(&self.type_name), env)?,
         self
            .instance_dtable
            .build_dtable(Rc::from(format!("type {}", &self.type_name).as_str()), env)?,
      ))
   }
}
