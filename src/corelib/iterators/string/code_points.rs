use crate::{builtin_traits::iterator, ll::value::RawValue, Engine, Error, TypeBuilder, UserData};

pub(crate) struct StringCodePoints {
    string: RawValue,
    index: usize,
}

impl StringCodePoints {
    pub unsafe fn new(s: RawValue) -> Self {
        Self { string: s, index: 0 }
    }

    fn has_next(&self) -> bool {
        self.index < unsafe { self.string.get_raw_string_unchecked().get().len() }
    }

    fn next(&mut self) -> Option<u32> {
        unsafe {
            let s = self.string.get_raw_string_unchecked().get();
            let ch = s[self.index..].chars().next();
            if let Some(ch) = ch {
                self.index += ch.len_utf8();
            }
            ch.map(u32::from)
        }
    }
}

impl UserData for StringCodePoints {}

pub(crate) fn load_string_code_points_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<StringCodePoints>::new("StringCodePoints")
            .add_builtin_trait_function(iterator::HasNext, StringCodePoints::has_next)
            .add_builtin_trait_function(iterator::Next, StringCodePoints::next),
    )?;

    Ok(())
}
