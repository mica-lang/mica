use std::{
    any::Any,
    borrow::Cow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

use super::{RawValue, UserData};
use crate::{
    ll::{
        bytecode::{DispatchTable, Library},
        error::LanguageErrorKind,
        gc::GcRaw,
    },
    Gc,
};

/// A Mica tuple.
pub struct Tuple {
    pub fields: Vec<RawValue>,
}

impl Tuple {
    /// Shorthand constructor for creating a tuple.
    pub fn new(fields: Vec<RawValue>) -> Self {
        Self { fields }
    }
}

impl fmt::Debug for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("(")?;
        for (i, &field) in self.fields.iter().enumerate() {
            if i != 0 {
                f.write_str(", ")?;
            }
            write!(f, "{field:?}")?;
        }
        if self.fields.len() == 1 {
            f.write_str(",)")?;
        } else {
            f.write_str(")")?;
        }

        Ok(())
    }
}

impl UserData for Tuple {
    fn dtable_gcraw(&self, library: Option<&Library>) -> GcRaw<DispatchTable> {
        Gc::as_raw(
            library
                .expect("UserData::dtable_gcraw called on a Tuple with no library")
                .builtin_dtables
                .tuples
                .get(self.fields.len())
                .and_then(|x| x.as_ref())
                .expect("dtable for tuple was not generated during compilation"),
        )
    }

    fn partial_eq(&self, other: &dyn UserData) -> bool {
        if let Some(tuple) = other.as_any().downcast_ref::<Tuple>() {
            self.fields == tuple.fields
        } else {
            false
        }
    }

    fn try_partial_cmp(&self, other: &dyn UserData) -> Result<Option<Ordering>, LanguageErrorKind> {
        if let Some(other) = other.as_any().downcast_ref::<Tuple>() {
            let (left, right) = (&self.fields[..], &other.fields[..]);
            let len = left.len().min(right.len());
            let a = &left[..len];
            let b = &right[..len];

            for i in 0..len {
                match a[i].try_partial_cmp(&b[i])? {
                    Some(Ordering::Equal) => (),
                    non_eq => return Ok(non_eq),
                }
            }

            Ok(left.len().partial_cmp(&right.len()))
        } else {
            Ok(None)
        }
    }

    fn hash(&self, mut hasher: &mut dyn Hasher) {
        self.fields.len().hash(&mut hasher);
        for &field in &self.fields {
            field.hash(&mut hasher);
        }
    }

    fn type_name(&self) -> Cow<'_, str> {
        Cow::Owned(format!("Tuple({})", self.fields.len()))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
