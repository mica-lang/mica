use std::{cell::UnsafeCell, cmp::Ordering};

use super::RawValue;
use crate::ll::error::LanguageErrorKind;

/// A Mica list.
#[derive(Debug)]
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
