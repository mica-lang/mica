use std::{
    any::Any,
    borrow::Cow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

use super::{RawValue, UserData};
use crate::{
    ll::{
        bytecode::{DispatchTable, Library, RecordType},
        error::LanguageErrorKind,
        gc::GcRaw,
    },
    Gc,
};

/// A Mica record.
pub struct Record {
    pub record_type: Rc<RecordType>,
    pub fields: Vec<RawValue>,
}

impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.fields.is_empty() {
            f.write_str("{}")?;
        } else {
            f.write_str("{ ")?;
            let mut field_names = self.record_type.identifier.split('+');
            for (i, &value) in self.fields.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                let field_name = field_names.next().expect("mismatch between number of fields in identifier and number of fields in record instance");
                write!(f, "{field_name}: {value:?}")?;
            }
            f.write_str(" }")?;
        }
        Ok(())
    }
}

impl UserData for Record {
    fn dtable_gcraw(&self, _: Option<&Library>) -> GcRaw<DispatchTable> {
        Gc::as_raw(&self.record_type.dtable)
    }

    fn partial_eq(&self, other: &dyn UserData) -> bool {
        if let Some(tuple) = other.as_any().downcast_ref::<Record>() {
            self.fields == tuple.fields
        } else {
            false
        }
    }

    fn try_partial_cmp(&self, _: &dyn UserData) -> Result<Option<Ordering>, LanguageErrorKind> {
        // Records do not form an order.
        Ok(None)
    }

    fn hash(&self, mut hasher: &mut dyn Hasher) {
        self.record_type.identifier.hash(&mut hasher);
        self.fields.len().hash(&mut hasher);
        for &field in &self.fields {
            field.hash(&mut hasher);
        }
    }

    fn type_name(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.record_type.dtable.pretty_name.deref())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
