use std::cmp::Ordering;
use std::rc::Rc;

use crate::ast::NodeId;
use crate::bytecode::{Function, Opr24};
use crate::common::ErrorKind;

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
   String(Rc<str>),
   Function(Rc<Closure>),
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

   pub fn boolean(&self) -> Result<bool, ErrorKind> {
      match self {
         Value::False => Ok(false),
         Value::True => Ok(true),
         _ => Err(ErrorKind::TypeError {
            expected: Type::Boolean,
            got: self.typ(),
         }),
      }
   }

   pub fn number(&self) -> Result<f64, ErrorKind> {
      if let &Value::Number(x) = self {
         Ok(x)
      } else {
         Err(ErrorKind::TypeError {
            expected: Type::Number,
            got: self.typ(),
         })
      }
   }

   pub fn string(&self) -> Result<&str, ErrorKind> {
      if let Value::String(s) = self {
         Ok(s)
      } else {
         Err(ErrorKind::TypeError {
            expected: Type::String,
            got: self.typ(),
         })
      }
   }

   pub fn function(&self) -> Result<&Rc<Closure>, ErrorKind> {
      if let Value::Function(c) = self {
         Ok(c)
      } else {
         Err(ErrorKind::TypeError {
            expected: Type::Function,
            got: self.typ(),
         })
      }
   }

   pub fn is_truthy(&self) -> bool {
      !matches!(self, Value::Nil | Value::False)
   }

   pub fn is_falsy(&self) -> bool {
      !self.is_truthy()
   }

   pub fn try_partial_cmp(&self, other: &Self) -> Result<Option<Ordering>, ErrorKind> {
      if self.typ() != other.typ() {
         Err(ErrorKind::TypeError {
            expected: self.typ(),
            got: other.typ(),
         })
      } else {
         match self {
            Self::Nil => Ok(Some(Ordering::Equal)),
            Self::False | Self::True => {
               Ok(Some(self.boolean().unwrap().cmp(&other.boolean().unwrap())))
            }
            Self::Number(x) => Ok(x.partial_cmp(&other.number().unwrap())),
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

impl std::fmt::Display for Value {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Value::Nil => f.write_str("nil"),
         Value::False => f.write_str("false"),
         Value::True => f.write_str("true"),
         Value::Number(x) => write!(f, "{x}"),
         Value::String(s) => write!(f, "{s}"),
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

pub struct Closure {
   pub function_id: Opr24,
}
