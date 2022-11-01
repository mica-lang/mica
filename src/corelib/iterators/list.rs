use crate::{builtin_traits::iterator, ll::value::RawValue, Engine, Error, TypeBuilder, UserData};

pub(crate) struct ListIter {
    list: RawValue,
    i: usize,
    len: usize,
}

impl ListIter {
    pub(crate) unsafe fn new(list: RawValue) -> Self {
        let slice = unsafe { list.get_raw_list_unchecked().get().as_slice() };
        ListIter { list, i: 0, len: slice.len() }
    }

    fn has_next(&self) -> bool {
        self.i < self.len
    }

    fn next(&mut self) -> Result<RawValue, LenChangedDuringIteration> {
        unsafe {
            let list = self.list.get_raw_list_unchecked().get();
            let slice = list.as_slice();
            if slice.len() != self.len {
                return Err(LenChangedDuringIteration { was: self.len, became: slice.len() });
            }
            let i = self.i;
            self.i += 1;
            Ok(slice[i])
        }
    }
}

impl UserData for ListIter {
    fn visit_references(&self, visit: &mut dyn FnMut(RawValue)) {
        visit(self.list);
    }
}

#[derive(Debug)]
struct LenChangedDuringIteration {
    was: usize,
    became: usize,
}

impl std::fmt::Display for LenChangedDuringIteration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "list length changed during iteration (was {}, became {})", self.was, self.became)
    }
}

impl std::error::Error for LenChangedDuringIteration {}

pub(crate) fn load_list_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<ListIter>::new("ListIter")
            .add_builtin_trait_function(iterator::HasNext, ListIter::has_next)
            .add_builtin_trait_function(iterator::Next, ListIter::next),
    )?;
    Ok(())
}
