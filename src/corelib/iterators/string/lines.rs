use crate::{builtin_traits::iterator, ll::value::RawValue, Engine, Error, TypeBuilder, UserData};

pub(crate) struct StringLines {
    string: RawValue,
    index: usize,
}

impl StringLines {
    pub unsafe fn new(s: RawValue) -> Self {
        Self { string: s, index: 0 }
    }

    fn has_next(&self) -> bool {
        self.index < unsafe { self.string.get_raw_string_unchecked().get().len() }
    }

    fn next(&mut self) -> Option<String> {
        unsafe {
            let s = self.string.get_raw_string_unchecked().get();
            let line = s[self.index..].lines().next();
            if let Some(line) = line {
                self.index += line.len();
            }
            // Since the resulting string does not count in the line separator, we have to handle
            // it ourselves here.
            if s[self.index..].starts_with("\r\n") {
                self.index += 2;
            } else if s[self.index..].starts_with('\n') {
                self.index += 1;
            }
            line.map(String::from)
        }
    }
}

impl UserData for StringLines {}

pub(crate) fn load_string_lines_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<StringLines>::new("StringLines")
            .add_builtin_trait_function(iterator::HasNext, StringLines::has_next)
            .add_builtin_trait_function(iterator::Next, StringLines::next),
    )?;

    Ok(())
}
