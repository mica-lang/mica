//! I/O facilities such as files.
//!
//! This feature is disabled by default as mica-std's I/O facilities are still under construction.

use std::io;

use mica::{Engine, MutSelfFromRawValue, SelfFromRawValue, TypeBuilder, UserData};

struct File {
    // The file is `None` if the user calls `close/0`.
    file: Option<std::fs::File>,
}

impl File {
    fn open(path: &str) -> io::Result<Self> {
        Ok(Self { file: Some(std::fs::File::open(path)?) })
    }

    fn close(&mut self) -> Result<(), FileAlreadyClosed> {
        if let Some(file) = self.file.take() {
            drop(file);
            Ok(())
        } else {
            Err(FileAlreadyClosed)
        }
    }
}

#[derive(Debug)]
struct FileAlreadyClosed;

impl std::fmt::Display for FileAlreadyClosed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("file is already closed")
    }
}

impl std::error::Error for FileAlreadyClosed {}

impl UserData for File {}

pub fn load_io(engine: &mut Engine) -> Result<(), mica::Error> {
    engine.add_type(
        TypeBuilder::<File>::new("File")
            .add_constructor("open", |ctor| {
                move |name: String| -> io::Result<_> { Ok(ctor.construct(File::open(&name)?)) }
            })
            .add_function("close", File::close),
    )?;

    Ok(())
}
