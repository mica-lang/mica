use crate::{builtin_traits::iterator, ll::value::RawValue, Engine, Error, TypeBuilder, UserData};

pub(crate) struct StringBytes {
    string: RawValue,
    index: usize,
}

impl StringBytes {
    pub unsafe fn new(s: RawValue) -> Self {
        Self { string: s, index: 0 }
    }

    fn has_next(&self) -> bool {
        self.index < unsafe { self.string.get_raw_string_unchecked().get().len() }
    }

    fn next(&mut self) -> Option<u8> {
        unsafe {
            let s = self.string.get_raw_string_unchecked().get().as_bytes();
            let byte = s.get(self.index).copied();
            self.index += 1;
            byte
        }
    }
}

impl UserData for StringBytes {}

pub(crate) fn load_string_bytes_iter(engine: &mut Engine) -> Result<(), Error> {
    engine.add_type(
        TypeBuilder::<StringBytes>::new("StringBytes")
            .add_builtin_trait_function(iterator::HasNext, StringBytes::has_next)
            .add_builtin_trait_function(iterator::Next, StringBytes::next),
    )?;

    Ok(())
}
