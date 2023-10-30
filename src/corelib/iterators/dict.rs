use hashbrown::raw::Bucket;

use crate::{
    builtin_traits::iterator,
    ll::value::{Dict, RawValue},
    Engine, Error, TypeBuilder, UserData,
};

type InnerIter = hashbrown::raw::RawIter<(RawValue, RawValue)>;

pub(crate) struct DictIter {
    dict: RawValue,
    iter: InnerIter,
    len: usize,
}

impl DictIter {
    pub(crate) unsafe fn new(value: RawValue) -> Self {
        let dict = value.downcast_user_data_unchecked::<Dict>();
        Self {
            dict: value,
            iter: dict.raw_iter(),
            len: dict.len(),
        }
    }

    fn checked_next(
        dict: RawValue,
        iter: &mut InnerIter,
        len: usize,
    ) -> Result<Option<Bucket<(RawValue, RawValue)>>, LenChangedDuringIteration> {
        // This length-must-not-change limitation exists because the dict must not reallocate,
        // otherwise the iterator becomes invalid.
        let current_len = unsafe { dict.downcast_user_data_unchecked::<Dict>().len() };
        if len != current_len {
            return Err(LenChangedDuringIteration {
                was: len,
                became: current_len,
            });
        }
        Ok(iter.next())
    }

    fn has_next(&self) -> Result<bool, LenChangedDuringIteration> {
        // Unfortunately this is a bit inefficient because we perform the iteration twice.
        Ok(Self::checked_next(self.dict, &mut self.iter.clone(), self.len)?.is_some())
    }

    fn next(&mut self) -> Result<Option<(RawValue, RawValue)>, LenChangedDuringIteration> {
        if let Some(bucket) = Self::checked_next(self.dict, &mut self.iter, self.len)? {
            let (key, value) = unsafe { bucket.read() };
            Ok(Some((key, value)))
        } else {
            Ok(None)
        }
    }
}

impl UserData for DictIter {
    fn visit_references(&self, visit: &mut dyn FnMut(RawValue)) {
        visit(self.dict);
    }
}

#[derive(Debug)]
struct LenChangedDuringIteration {
    was: usize,
    became: usize,
}

impl std::fmt::Display for LenChangedDuringIteration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "dict length changed during iteration (was {}, became {})",
            self.was, self.became
        )
    }
}

impl std::error::Error for LenChangedDuringIteration {}

pub(crate) fn load_dict_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<DictIter>::new("DictIter")
            .add_builtin_trait_function(iterator::HasNext, DictIter::has_next)
            .add_builtin_trait_function(iterator::Next, DictIter::next),
    )?;

    Ok(())
}
