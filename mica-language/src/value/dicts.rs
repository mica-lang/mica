use std::cell::UnsafeCell;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

use nohash_hasher::BuildNoHashHasher;

use super::{RawValue, ValueKind};

type DictMap = HashMap<u64, (RawValue, RawValue), BuildNoHashHasher<u64>>;

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

   /// Sets the value at the given key. Returns an error if the key cannot be hashed.
   pub fn insert(&self, key: RawValue, value: RawValue) {
      unsafe {
         let inner = &mut *self.inner.get();
         inner.pairs.insert(key.hash(inner.state.build_hasher()), (key, value));
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
            // TODO: Consider not hashing lists by reference.
            ValueKind::List => self.get_raw_list_unchecked().get_raw().hash(&mut state),
            ValueKind::UserData => self.get_raw_user_data_unchecked().get_raw().hash(&mut state),
         }
      }
      state.finish()
   }
}
