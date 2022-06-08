use std::cell::UnsafeCell;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

use nohash_hasher::BuildNoHashHasher;

use super::{RawValue, ValueKind};

pub type DictMap = HashMap<u64, (RawValue, RawValue), BuildNoHashHasher<u64>>;

#[derive(Default)]
struct DictInner {
   pairs: DictMap,
   state: RandomState,
}

/// A dict (dictionary) storing arbitrarily typed keys and values.
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
      unsafe {
         let inner = &*self.inner.get();
         inner.pairs.len()
      }
   }

   /// Returns whether the dict is empty.
   pub fn is_empty(&self) -> bool {
      self.len() == 0
   }

   /// Sets the value at the given key. Returns an error if the key cannot be hashed.
   pub fn insert(&self, key: RawValue, value: RawValue) {
      unsafe {
         let inner = &mut *self.inner.get();
         inner.pairs.insert(key.hash(inner.state.build_hasher()), (key, value));
      }
   }

   /// Removes the value at the given key and returns it (or `nil` if there was no value).
   pub fn remove(&self, key: RawValue) -> RawValue {
      unsafe {
         let inner = &mut *self.inner.get();
         inner
            .pairs
            .remove(&key.hash(inner.state.build_hasher()))
            .map(|(_k, v)| v)
            .unwrap_or(RawValue::from(()))
      }
   }

   /// Returns the value at the given key. Returns an error if the key cannot be hashed.
   pub fn get(&self, key: RawValue) -> RawValue {
      unsafe {
         let inner = &*self.inner.get();
         inner
            .pairs
            .get(&key.hash(inner.state.build_hasher()))
            .map(|&(_k, v)| v)
            .unwrap_or(RawValue::from(()))
      }
   }

   /// Returns whether the dict contains a value under the given key.
   pub fn contains_key(&self, key: RawValue) -> bool {
      unsafe {
         let inner = &*self.inner.get();
         inner.pairs.contains_key(&key.hash(inner.state.build_hasher()))
      }
   }

   /// Returns an iterator over pairs stored in the dict.
   ///
   /// WARNING: The dict must not be modified while iterating over it.
   pub(crate) fn iter(&self) -> impl Iterator<Item = (RawValue, RawValue)> + '_ {
      unsafe {
         let inner = &*self.inner.get();
         inner.pairs.iter().map(|(_, &(k, v))| (k, v))
      }
   }
}

impl RawValue {
   /// Hashes the value. If the value is not hashable, returns an error.
   /// Note that this is not part of a `std::hash::Hash` implementation because this may get extra
   /// arguments in the future to support overloading hashing from the language itself.
   pub fn hash<H>(&self, mut state: H) -> u64
   where
      H: Hasher,
   {
      self.kind().hash(&mut state);
      unsafe {
         match self.kind() {
            // Primitives are hashed by value.
            ValueKind::Nil => (),
            ValueKind::Boolean => self.get_boolean_unchecked().hash(&mut state),
            ValueKind::Number => self.get_number_unchecked().to_bits().hash(&mut state),
            ValueKind::String => self.get_raw_string_unchecked().get().hash(&mut state),
            // Objects with interior mutability are hashed by reference.
            ValueKind::Function => self.get_raw_function_unchecked().get_raw().hash(&mut state),
            ValueKind::Struct => self.get_raw_struct_unchecked().get_raw().hash(&mut state),
            // TODO: Consider not hashing lists and dicts by reference.
            ValueKind::List => self.get_raw_list_unchecked().get_raw().hash(&mut state),
            ValueKind::Dict => self.get_raw_dict_unchecked().get_raw().hash(&mut state),
            ValueKind::UserData => self.get_raw_user_data_unchecked().get_raw().hash(&mut state),
         }
      }
      state.finish()
   }
}
