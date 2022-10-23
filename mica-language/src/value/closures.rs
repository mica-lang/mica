use std::{cell::UnsafeCell, fmt, marker::PhantomPinned, mem, pin::Pin, ptr, rc::Rc};

use super::RawValue;
use crate::bytecode::Opr24;

/// An upvalue captured by a closure.
pub struct Upvalue {
    /// A writable pointer to the variable captured by this upvalue.
    pub(crate) ptr: UnsafeCell<ptr::NonNull<RawValue>>,
    /// Storage for a closed upvalue.
    closed: UnsafeCell<RawValue>,

    _pinned: PhantomPinned,
}

impl fmt::Debug for Upvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Upvalue")
            .field("ptr", unsafe { self.ptr.get().as_ref() }.unwrap())
            .field("closed", unsafe { self.closed.get().as_ref() }.unwrap())
            .finish_non_exhaustive()
    }
}

impl Upvalue {
    /// Creates a new upvalue pointing to a live variable.
    pub(crate) fn new(var: ptr::NonNull<RawValue>) -> Pin<Rc<Upvalue>> {
        Rc::pin(Upvalue {
            ptr: UnsafeCell::new(var),
            closed: UnsafeCell::new(RawValue::from(())),
            _pinned: PhantomPinned,
        })
    }

    /// Closes an upvalue by `mem::take`ing the value behind the `ptr` into the `closed` field, and
    /// updating the `ptr` field to point to the `closed` field's contents.
    ///
    /// # Safety
    /// The caller must ensure there are no mutable references to the variable at the time of
    /// calling this.
    pub(crate) unsafe fn close(&self) {
        let ptr = &mut *self.ptr.get();
        let closed = &mut *self.closed.get();
        let value = mem::take(ptr.as_mut());
        *closed = value;
        *ptr = ptr::NonNull::new(closed).unwrap();
    }

    /// Returns the value pointed to by this upvalue.
    ///
    /// # Safety
    /// The caller must ensure there are no mutable references to the source variable at the time
    /// of calling this.
    pub(crate) unsafe fn get(&self) -> RawValue {
        *(*self.ptr.get()).as_ptr()
    }

    /// Writes to the variable pointed to by this upvalue.
    ///
    /// # Safety
    /// The caller must ensure there are no mutable references to the source variable at the time
    /// of calling this.
    pub(crate) unsafe fn set(&self, value: RawValue) {
        *(*self.ptr.get()).as_ptr() = value;
    }
}

/// The runtime representation of a function.
#[derive(Debug)]
#[repr(align(8))]
pub struct Closure {
    pub name: Rc<str>,
    pub function_id: Opr24,
    pub captures: Vec<Pin<Rc<Upvalue>>>,
}
