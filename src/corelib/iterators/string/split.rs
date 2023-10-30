use crate::{
    builtin_traits::iterator, ll::value::RawValue, Engine, Error, Gc, TypeBuilder, UserData,
};

pub(crate) struct StringSplit {
    string: RawValue,
    separator: Gc<String>,
    index: usize,
}

impl StringSplit {
    pub unsafe fn new(s: RawValue, separator: Gc<String>) -> Self {
        Self {
            string: s,
            separator,
            index: 0,
        }
    }

    fn has_next(&self) -> bool {
        self.index < unsafe { self.string.get_raw_string_unchecked().get().len() }
    }

    fn next(&mut self) -> Option<String> {
        unsafe {
            let s = self.string.get_raw_string_unchecked().get();
            let fragment = s[self.index..].split(&**self.separator).next();
            if let Some(fragment) = fragment {
                self.index += fragment.len();
                self.index += self.separator.len();
            }
            fragment.map(String::from)
        }
    }
}

impl UserData for StringSplit {}

pub(crate) fn load_string_split_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<StringSplit>::new("StringSplit")
            .add_builtin_trait_function(iterator::HasNext, StringSplit::has_next)
            .add_builtin_trait_function(iterator::Next, StringSplit::next),
    )?;

    Ok(())
}
