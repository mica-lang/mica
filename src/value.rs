use std::cmp::Ordering;

use crate::common::{Error, ErrorKind, Location};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
   Nil,
   Boolean,
   Number,
   String,
}

impl std::fmt::Display for Type {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{self:?}")
   }
}

#[derive(Clone, PartialEq)]
pub enum Value {
   Nil,
   False,
   True,
   Number(f64),
   String(String),
}

impl Value {
   pub fn typ(&self) -> Type {
      match self {
         Value::Nil => Type::Nil,
         Value::False | Value::True => Type::Boolean,
         Value::Number(_) => Type::Number,
         Value::String(_) => Type::String,
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
      }
   }
}
