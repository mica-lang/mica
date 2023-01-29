use crate::{builtin_traits::iterator, ll::value::RawValue, Engine, Error, TypeBuilder, UserData};

pub(crate) struct StringChars {
    string: RawValue,
    index: usize,
}

impl StringChars {
    pub unsafe fn new(s: RawValue) -> Self {
        Self { string: s, index: 0 }
    }

    fn has_next(&self) -> bool {
        self.index < unsafe { self.string.get_raw_string_unchecked().get().len() }
    }

    fn next(&mut self) -> Option<char> {
        unsafe {
            let s = self.string.get_raw_string_unchecked().get();
            let ch = s[self.index..].chars().next();
            if let Some(ch) = ch {
                self.index += ch.len_utf8();
            }
            ch
        }
    }
}

impl UserData for StringChars {}

pub(crate) fn load_string_chars_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<StringChars>::new("StringChars")
            .add_builtin_trait_function(iterator::HasNext, StringChars::has_next)
            .add_builtin_trait_function(iterator::Next, StringChars::next),
    )?;

    Ok(())
}
