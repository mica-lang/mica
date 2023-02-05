use std::{
    any::Any,
    borrow::Cow,
    cell::UnsafeCell,
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

/// A Mica list.
pub struct List {
    elements: UnsafeCell<Vec<RawValue>>,
}

impl List {
    /// Creates a new, empty list.
    pub fn new(elements: Vec<RawValue>) -> List {
        List { elements: UnsafeCell::new(elements) }
    }

    /// Returns a mutable reference to the vector inside.
    ///
    /// # Safety
    /// No references (mutable or not) to the vector must exist at the time of calling this.
    pub unsafe fn get_mut(&self) -> *mut Vec<RawValue> {
        self.elements.get()
    }

    /// Returns the items of the list as a slice.
    ///
    /// # Safety
    /// There must be no mutable references to the list inside at the time of calling this.
    pub(crate) unsafe fn as_slice(&self) -> &[RawValue] {
        &*self.elements.get()
    }

    /// Attempts to compare two lists to each other lexicographically.
    pub(crate) unsafe fn try_partial_cmp(
        &self,
        other: &List,
    ) -> Result<Option<Ordering>, LanguageErrorKind> {
        let (left, right) = (self.as_slice(), other.as_slice());
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
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        (unsafe { &*self.elements.get() } == unsafe { &*other.elements.get() })
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        unsafe {
            for (i, &element) in self.as_slice().iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{element}")?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl UserData for List {
    fn dtable_gcraw(&self, library: Option<&Library>) -> GcRaw<DispatchTable> {
        Gc::as_raw(
            &library
                .expect("UserData::dtable_gcraw called on List with no environment")
                .builtin_dtables
                .list,
        )
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn partial_eq(&self, other: &dyn UserData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<List>() {
            self == other
        } else {
            false
        }
    }

    fn try_partial_cmp(&self, other: &dyn UserData) -> Result<Option<Ordering>, LanguageErrorKind> {
        if let Some(other) = other.as_any().downcast_ref::<List>() {
            unsafe { self.try_partial_cmp(other) }
        } else {
            Err(LanguageErrorKind::TypeError {
                expected: "List".into(),
                got: other.type_name().into_owned().into(),
            })
        }
    }

    fn hash(&self, mut hasher: &mut dyn Hasher) {
        unsafe {
            let slice = self.as_slice();
            slice.len().hash(&mut hasher);
            for &element in slice {
                element.hash(&mut hasher);
            }
        }
    }

    fn type_name(&self) -> Cow<'_, str> {
        Cow::Borrowed("List")
    }

    fn visit_references(&self, visit: &mut dyn FnMut(RawValue)) {
        unsafe {
            for &value in self.as_slice() {
                visit(value);
            }
        }
    }
}
