use std::{
    any::Any,
    cell::{Cell, UnsafeCell},
    cmp::Ordering,
    fmt,
    hash::Hasher,
    marker::PhantomData,
};

use crate::{
    ll::{
        bytecode::{DispatchTable, Library},
        error::LanguageErrorKind,
        gc::{Gc, GcRaw},
        value::{self, RawValue},
    },
    Error,
};

/// Marker trait for all user data types.
///
/// Due to limitations in Rust's type system each user-defined type must implement this.
pub trait UserData: Any {
    /// Used to let the GC know of any [`RawValue`]s referenced by the data.
    ///
    /// Normally, referencing [`RawValue`]s inside of user data is unsafe, because they may be
    /// garbage collected at any time. There is no way for you to know whether a [`RawValue`]
    /// has been collected or not.
    ///
    /// So to allow the GC to see any [`RawValue`] references inside user data, this must be
    /// overridden. `visit` should be called for each [`RawValue`] stored inside the user data, so
    /// that the GC can mark the references as reachable.
    ///
    /// It's usually better/easier to deal with strong [`Value`][crate::Value]s which use
    /// reference counting to manage allocations.
    ///
    /// # Examples
    /// ```
    /// use mica::{UserData, ll::value::RawValue};
    ///
    /// struct MyVec {
    ///     elements: Vec<RawValue>,
    /// }
    ///
    /// impl UserData for MyVec {
    ///     fn visit_references(&self, visit: &mut dyn FnMut(RawValue)) {
    ///         for &element in &self.elements {
    ///             visit(element);
    ///         }
    ///     }
    /// }
    /// ```
    #[allow(unused_variables)]
    fn visit_references(&self, visit: &mut dyn FnMut(RawValue)) {}
}

/// A type. This is used to represent user-defined Rust types in the VM (but not their instances).
pub(crate) struct Type<T> {
    dtable: Gc<DispatchTable>,
    _data: PhantomData<T>,
}

impl<T> Type<T> {
    pub(crate) fn new(dtable: Gc<DispatchTable>) -> Self {
        Self { dtable, _data: PhantomData }
    }
}

impl<T> fmt::Debug for Type<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<[{}]>", self.dtable.pretty_name)
    }
}

impl<T> value::UserData for Type<T>
where
    T: Any,
{
    fn dtable_gcraw(&self, _: Option<&Library>) -> GcRaw<DispatchTable> {
        Gc::as_raw(&self.dtable)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn partial_eq(&self, other: &dyn value::UserData) -> bool {
        self as *const dyn value::UserData == other
    }

    fn try_partial_cmp(
        &self,
        _other: &dyn value::UserData,
    ) -> Result<Option<Ordering>, LanguageErrorKind> {
        Ok(None)
    }

    fn hash(&self, mut hasher: &mut dyn Hasher) {
        std::ptr::hash(self, &mut hasher);
    }

    // This does return the correct name, clippy.
    #[allow(clippy::misnamed_getters)]

    fn type_name(&self) -> &str {
        &self.dtable.pretty_name
    }
}

/// Represents a custom-typed value stored inside a VM.
///
/// Built-in types such as [`f64`] shouldn't be used as the `T` of an `Object`, because they are
/// can be represented by values inherently. `Object` however may be used for binding types that are
/// not directly supported by the VM, like [`std::fs::File`].
pub(crate) struct Object<T> {
    pub(crate) dtable: Gc<DispatchTable>,
    // The functionality of the RefCell unfortunately has to be replicated because we need unsafe
    // guards that the standard RefCell doesn't provide.
    shared_borrows: Cell<usize>,
    borrowed_mutably: Cell<bool>,
    data: UnsafeCell<T>,
}

impl<T> Object<T> {
    pub(crate) fn new(dtable: GcRaw<DispatchTable>, data: T) -> Self {
        Self {
            dtable: unsafe { Gc::from_raw(dtable) },
            shared_borrows: Cell::new(0),
            borrowed_mutably: Cell::new(false),
            data: UnsafeCell::new(data),
        }
    }

    /// Borrows the object as immutably using an unsafe guard.
    ///
    /// # Safety
    /// This function is unsafe because the guard can be dropped accidentally while keeping the
    /// reference alive, which can lead to two mutable references to the data existing at once.
    #[doc(hidden)]
    pub(crate) unsafe fn unsafe_borrow(&self) -> Result<(&T, UnsafeRefGuard<T>), Error> {
        if self.borrowed_mutably.get() {
            return Err(Error::ReentrantMutableBorrow);
        }
        self.shared_borrows.set(self.shared_borrows.get() + 1);
        let reference = &*self.data.get();
        Ok((reference, UnsafeRefGuard { object: self as *const _ }))
    }

    /// Borrows the object mutably using an unsafe guard.
    ///
    /// # Safety
    /// This function is unsafe because the guard can be dropped accidentally while keeping the
    /// reference alive, which can lead to two mutable references to the data existing at once.
    #[doc(hidden)]
    pub(crate) unsafe fn unsafe_borrow_mut(&self) -> Result<(&mut T, UnsafeMutGuard<T>), Error> {
        if self.shared_borrows.get() > 0 {
            return Err(Error::ReentrantMutableBorrow);
        }
        self.borrowed_mutably.set(true);
        let reference = &mut *self.data.get();
        Ok((reference, UnsafeMutGuard { object: self as *const _ }))
    }
}

impl<T> fmt::Debug for Object<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<[{}]>", self.dtable.pretty_name)
    }
}

impl<T> value::UserData for Object<T>
where
    T: Any,
{
    fn dtable_gcraw(&self, _: Option<&Library>) -> GcRaw<DispatchTable> {
        Gc::as_raw(&self.dtable)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn partial_eq(&self, other: &dyn value::UserData) -> bool {
        self as *const dyn value::UserData == other
    }

    fn try_partial_cmp(
        &self,
        _other: &dyn value::UserData,
    ) -> Result<Option<Ordering>, LanguageErrorKind> {
        Ok(None)
    }

    fn hash(&self, mut hasher: &mut dyn Hasher) {
        std::ptr::hash(self, &mut hasher);
    }

    // This does return the correct name, clippy.
    #[allow(clippy::misnamed_getters)]
    fn type_name(&self) -> &str {
        &self.dtable.pretty_name
    }
}

/// An _unsafe_ guard for a `&T` borrowed from an `Object<T>`.
///
/// This is an **unsafe** guard because it must not outlive the `Object<T>` it guards, but that
/// is not guaranteed using the borrow checker. This type is needed because generic associated types
/// haven't been stabilized yet.
#[doc(hidden)]
#[derive(Debug)]
pub struct UnsafeRefGuard<T> {
    object: *const Object<T>,
}

impl<T> Drop for UnsafeRefGuard<T> {
    fn drop(&mut self) {
        unsafe {
            let object = &*self.object;
            object.shared_borrows.set(object.shared_borrows.get() - 1);
        }
    }
}

/// An _unsafe_ guard for a `&mut T` borrowed from an `Object<T>`.
///
/// This is an **unsafe** guard because it must not outlive the `Object<T>` it guards, but that
/// is not guaranteed using the borrow checker. This type is needed because generic associated types
/// haven't been stabilized yet.
#[doc(hidden)]
#[derive(Debug)]
pub struct UnsafeMutGuard<T> {
    object: *const Object<T>,
}

impl<T> Drop for UnsafeMutGuard<T> {
    fn drop(&mut self) {
        unsafe {
            let object = &*self.object;
            object.borrowed_mutably.set(false);
        }
    }
}
