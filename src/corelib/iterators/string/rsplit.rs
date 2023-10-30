use crate::{
    builtin_traits::iterator, ll::value::RawValue, Engine, Error, Gc, TypeBuilder, UserData,
};

pub(crate) struct StringRSplit {
    string: RawValue,
    separator: Gc<String>,
    index: usize,
}

impl StringRSplit {
    pub unsafe fn new(s: RawValue, separator: Gc<String>) -> Self {
        Self {
            string: s,
            separator,
            index: unsafe { s.get_raw_string_unchecked().get().len() },
        }
    }

    fn has_next(&self) -> bool {
        self.index != 0
    }

    fn next(&mut self) -> Option<String> {
        unsafe {
            let s = self.string.get_raw_string_unchecked().get();
            let fragment = s[0..self.index].rsplit(&**self.separator).next();
            if let Some(fragment) = fragment {
                self.index -= fragment.len();
                self.index = self.index.saturating_sub(self.separator.len());
            }
            fragment.map(String::from)
        }
    }
}

impl UserData for StringRSplit {}

pub(crate) fn load_string_rsplit_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<StringRSplit>::new("StringRSplit")
            .add_builtin_trait_function(iterator::HasNext, StringRSplit::has_next)
            .add_builtin_trait_function(iterator::Next, StringRSplit::next),
    )?;

    Ok(())
}
