//! Implementation of the `Dict` type based on hashbrown.

use std::{
    cell::UnsafeCell,
    cmp::Ordering,
    collections::hash_map::RandomState,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    mem,
};

use hashbrown::raw::RawTable;

use super::{RawValue, UserData, ValueKind};
use crate::{
    ll::{
        bytecode::{DispatchTable, Library},
        error::LanguageErrorKind,
        gc::GcRaw,
    },
    Gc,
};

type DictMap = RawTable<(RawValue, RawValue)>;
type DictHashBuilder = RandomState;

#[derive(Default, Clone)]
struct DictInner {
    table: DictMap,
    state: DictHashBuilder,
}

/// A dict (dictionary) storing arbitrarily typed keys and values.
///
/// Note that this type has interior mutability. This is because dicts in Mica are shared by
/// reference; creating a new dict requires using `clone`.
#[derive(Default)]
pub struct Dict {
    inner: UnsafeCell<DictInner>,
}

impl Dict {
    /// Creates a new dict.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of elements stored in the dict.
    pub fn len(&self) -> usize {
        let inner = unsafe { &*self.inner.get() };
        inner.table.len()
    }

    /// Returns whether the dict is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Sets the value at the given key. Returns the old value, or `nil` if there was no value.
    pub fn insert(&self, key: RawValue, value: RawValue) -> RawValue {
        let inner = unsafe { &mut *self.inner.get() };
        let hasher = make_hasher(&inner.state);
        let key_hash = hasher(&(key, value));
        if let Some((_, item)) = inner.table.get_mut(key_hash, equivalent_key(key)) {
            mem::replace(item, value)
        } else {
            inner.table.insert(key_hash, (key, value), hasher);
            RawValue::from(())
        }
    }

    /// Removes the value at the given key and returns it (or `nil` if there was no value).
    pub fn remove(&self, key: RawValue) -> RawValue {
        let inner = unsafe { &mut *self.inner.get() };
        match inner
            .table
            .remove_entry(key.hash(&mut inner.state.build_hasher()), equivalent_key(key))
        {
            Some((_, v)) => v,
            None => RawValue::from(()),
        }
    }

    /// Returns the value under the given key, or `None` if there is no such value.
    pub fn get(&self, key: RawValue) -> Option<RawValue> {
        let inner = unsafe { &*self.inner.get() };
        if inner.table.is_empty() {
            None
        } else {
            let hash = key.hash(&mut inner.state.build_hasher());
            inner.table.get(hash, equivalent_key(key)).map(|&(_key, value)| value)
        }
    }

    /// Returns whether the dict contains a value under the given key.
    pub fn contains_key(&self, key: RawValue) -> bool {
        self.get(key).is_some()
    }

    /// Returns an iterator over pairs stored in the dict.
    ///
    /// # Safety
    /// The dict must not be modified while iterating over it. The iterator must not outlive the
    /// dict.
    pub(crate) unsafe fn iter(&self) -> impl Iterator<Item = (RawValue, RawValue)> + '_ {
        let inner = &*self.inner.get();
        let iterator = inner.table.iter();
        iterator.map(|bucket| bucket.read())
    }

    /// Returns a raw iterator over pairs stored in the dict.
    ///
    /// # Safety
    /// The iterator must not outlive the dict.
    pub(crate) unsafe fn raw_iter(&self) -> hashbrown::raw::RawIter<(RawValue, RawValue)> {
        let inner = &*self.inner.get();
        inner.table.iter()
    }
}

impl Clone for Dict {
    fn clone(&self) -> Self {
        Self { inner: UnsafeCell::new((unsafe { &*self.inner.get() }).clone()) }
    }
}

impl PartialEq for Dict {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        unsafe { self.iter() }.all(|(key, value)| other.get(key).map_or(false, |v| value == v))
    }
}

impl fmt::Debug for Dict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "[:]")?;
        } else {
            write!(f, "[")?;
            unsafe {
                for (i, (key, value)) in self.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl UserData for Dict {
    fn dtable_gcraw(&self, library: Option<&Library>) -> GcRaw<DispatchTable> {
        Gc::as_raw(
            &library
                .expect("UserData::dtable_gcraw called on a Dict with no library")
                .builtin_dtables
                .dict,
        )
    }

    fn partial_eq(&self, other: &dyn UserData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Dict>() {
            self == other
        } else {
            false
        }
    }

    fn try_partial_cmp(
        &self,
        _other: &dyn UserData,
    ) -> Result<Option<Ordering>, LanguageErrorKind> {
        Ok(None)
    }

    fn hash(&self, mut hasher: &mut dyn Hasher) {
        unsafe {
            for (key, value) in self.iter() {
                key.hash(&mut hasher);
                value.hash(&mut hasher);
            }
        }
    }

    fn type_name(&self) -> &str {
        "Dict"
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl RawValue {
    /// Hashes the value.
    /// Note that this is not part of a `std::hash::Hash` implementation because this may get extra
    /// arguments in the future to support overloading hashing from the language itself.
    pub fn hash<H>(&self, state: &mut H) -> u64
    where
        H: Hasher,
    {
        self.kind().hash(state);
        unsafe {
            match self.kind() {
                // Primitives are hashed by value.
                ValueKind::Nil => (),
                ValueKind::Boolean => self.get_boolean_unchecked().hash(state),
                // Hashing floats isn't normally considered OK by Rust, but in our case it's the
                // only option because we don't have an integer type (and probably
                // never will).
                ValueKind::Number => self.get_number_unchecked().to_bits().hash(state),
                ValueKind::String => self.get_raw_string_unchecked().get().hash(state),
                // Objects with interior mutability are hashed by reference.
                ValueKind::Function => self.get_raw_function_unchecked().get_raw().hash(state),
                ValueKind::Struct => self.get_raw_struct_unchecked().get_raw().hash(state),
                ValueKind::Trait => self.get_raw_trait_unchecked().get_raw().hash(state),
                // Except for user data, which have its own hash().
                ValueKind::UserData => self.get_raw_user_data_unchecked().get().hash(state),
            }
        }
        state.finish()
    }
}

fn make_hasher(builder: &DictHashBuilder) -> impl Fn(&(RawValue, RawValue)) -> u64 + '_ {
    move |&(key, _value)| key.hash(&mut builder.build_hasher())
}

fn equivalent_key(key: RawValue) -> impl Fn(&(RawValue, RawValue)) -> bool {
    move |&(key2, _value)| key == key2
}
