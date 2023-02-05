use std::fmt::Display;

mod functions;
mod stress;
mod traits;
mod value;

pub trait RevealResultExt<T> {
    /// Basically the same as `unwrap()` but `Display`s the error instead of `Debug`ging it.
    fn reveal(self) -> T;
}

impl<T, E> RevealResultExt<T> for Result<T, E>
where
    E: Display,
{
    fn reveal(self) -> T {
        match self {
            Ok(ok) => ok,
            Err(error) => {
                panic!("Err result revealed:\n\n{error}\n\n");
            }
        }
    }
}
