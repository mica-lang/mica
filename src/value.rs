use std::cmp::Ordering;
use std::rc::Rc;

use crate::ast::Node;
use crate::common::{Error, ErrorKind, Location};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
   Nil,
   Boolean,
   Number,
   String,
   Function,
}

impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{self:?}")
   }
}

#[derive(Clone)]
pub enum Value {
   Nil,
   False,
   True,
   Number(f64),
   String(String),
   Function(Rc<Function>),
}

impl Value {
   pub fn typ(&self) -> Type {
      match self {
         Value::Nil => Type::Nil,
         Value::False | Value::True => Type::Boolean,
         Value::Number(_) => Type::Number,
         Value::String(_) => Type::String,
         Value::Function(_) => Type::Function,
      }
   }

   pub fn boolean(&self, location: Location) -> Result<bool, Error> {
      match self {
         Value::False => Ok(false),
         Value::True => Ok(true),
         _ => Err(Error {
            kind: ErrorKind::TypeError {
               expected: Type::Boolean,
               got: self.typ(),
            },
            location,
         }),
      }
   }

   pub fn number(&self, location: Location) -> Result<f64, Error> {
      if let &Value::Number(x) = self {
         Ok(x)
      } else {
         Err(Error {
            kind: ErrorKind::TypeError {
               expected: Type::Number,
               got: self.typ(),
            },
            location,
         })
      }
   }

   pub fn string(&self, location: Location) -> Result<&str, Error> {
      if let Value::String(s) = self {
         Ok(s)
      } else {
         Err(Error {
            kind: ErrorKind::TypeError {
               expected: Type::String,
               got: self.typ(),
            },
            location,
         })
      }
   }

   pub fn is_truthy(&self) -> bool {
      !matches!(self, Value::Nil | Value::False)
   }

   pub fn is_falsy(&self) -> bool {
      !self.is_truthy()
   }

   pub fn try_partial_cmp(
      &self,
      other: &Self,
      location: Location,
   ) -> Result<Option<Ordering>, Error> {
      if self.typ() != other.typ() {
         Err(Error {
            kind: ErrorKind::TypeError {
               expected: self.typ(),
               got: other.typ(),
            },
            location,
         })
      } else {
         match self {
            Self::Nil => Ok(Some(Ordering::Equal)),
            Self::False | Self::True => Ok(Some(
               self.boolean(location).unwrap().cmp(&other.boolean(location).unwrap()),
            )),
            Self::Number(x) => Ok(x.partial_cmp(&other.number(location).unwrap())),
            Self::String(s) => {
               if let Value::String(t) = &other {
                  Ok(s.partial_cmp(t))
               } else {
                  unreachable!()
               }
            }
            Self::Function(_) => Ok(None),
         }
      }
   }
}

impl From<bool> for Value {
   fn from(b: bool) -> Self {
      match b {
         false => Self::False,
         true => Self::True,
      }
   }
}

impl std::fmt::Debug for Value {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Value::Nil => f.write_str("nil"),
         Value::False => f.write_str("false"),
         Value::True => f.write_str("true"),
         Value::Number(x) => write!(f, "{x}"),
         Value::String(s) => write!(f, "{s:?}"),
         Value::Function(_) => write!(f, "<func>"),
      }
   }
}

impl PartialEq for Value {
   fn eq(&self, other: &Self) -> bool {
      match (self, other) {
         (Self::Number(l), Self::Number(r)) => l == r,
         (Self::String(l), Self::String(r)) => l == r,
         (Self::Function(l), Self::Function(r)) => Rc::ptr_eq(l, r),
         _ => core::mem::discriminant(self) == core::mem::discriminant(other),
      }
   }
}

#[derive(Debug, Clone)]
pub struct Function {
   pub parameters: Vec<String>,
   pub body: Vec<Box<Node>>,
}
