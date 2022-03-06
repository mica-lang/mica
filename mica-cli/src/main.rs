use std::path::PathBuf;

use mica::{Engine, Error, LanguageError, LanguageErrorKind, Value};
use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Helper};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "mica")]
struct Opts {
   file: Option<PathBuf>,
}

struct MicaValidator;

impl Helper for MicaValidator {}

impl Hinter for MicaValidator {
   type Hint = String;
}

impl Highlighter for MicaValidator {}

impl Completer for MicaValidator {
   type Candidate = String;
}

impl Validator for MicaValidator {
   fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
      let engine = Engine::new();
      if let Err(error) = engine.compile("(repl)", ctx.input()) {
         use LanguageErrorKind as ErrorKind;
         if let Error::Compile(LanguageError::Compile {
            kind:
               ErrorKind::MissingEnd | ErrorKind::MissingClosingQuote | ErrorKind::MissingRightParen,
            ..
         }) = error
         {
            return Ok(ValidationResult::Incomplete);
         }
      }
      Ok(ValidationResult::Valid(None))
   }
}

fn interpret(engine: &Engine, filename: &str, input: String) -> impl Iterator<Item = Value> {
   let mut fiber = match engine.start(filename, input) {
      Ok(fiber) => fiber,
      Err(error) => {
         eprintln!("{error}");
         return None.into_iter().flatten();
      }
   };
   Some(std::iter::from_fn(move || match fiber.resume() {
      Ok(value) => value,
      Err(error) => {
         eprintln!("{error}");
         None
      }
   }))
   .into_iter()
   .flatten()
}

fn repl() -> Result<(), mica::Error> {
   println!("Mica {} REPL", env!("CARGO_PKG_VERSION"));
   println!("Press Ctrl-C to exit.");
   println!();

   let mut editor =
      Editor::with_config(rustyline::Config::builder().auto_add_history(true).build());
   editor.set_helper(Some(MicaValidator));

   let engine = Engine::new();
   while let Ok(line) = editor.readline("> ") {
      for result in interpret(&engine, "(repl)", line) {
         println!("< {result:?}");
         println!();
      }
   }

   Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
   let opts = Opts::from_args();
   if let Some(path) = &opts.file {
      let file = std::fs::read_to_string(path)?;
      let engine = Engine::new();
      let _ = interpret(&engine, path.to_str().unwrap(), file);
   } else {
      repl()?;
   }

   Ok(())
}
