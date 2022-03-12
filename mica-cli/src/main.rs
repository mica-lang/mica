use std::fmt;
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
struct Options {
   file: Option<PathBuf>,

   #[structopt(flatten)]
   engine_options: EngineOptions,
}

#[derive(StructOpt)]
struct EngineOptions {
   #[structopt(long)]
   dump_ast: bool,
   #[structopt(long)]
   dump_bytecode: bool,
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

fn interpret(
   engine: &Engine,
   filename: &str,
   input: String,
) -> Result<impl Iterator<Item = Result<Value, mica::Error>>, mica::Error> {
   let mut fiber = match engine.start(filename, input) {
      Ok(fiber) => fiber,
      Err(error) => {
         eprintln!("{error}");
         return Ok(None.into_iter().flatten());
      }
   };
   Ok(Some(std::iter::from_fn(move || match fiber.resume() {
      Ok(Some(value)) => Some(Ok(value)),
      Ok(None) => None,
      Err(error) => {
         eprintln!("{error}");
         Some(Err(error))
      }
   }))
   .into_iter()
   .flatten())
}

fn engine(options: &EngineOptions) -> Result<Engine, mica::Error> {
   let engine = Engine::with_debug_options(mica::DebugOptions {
      dump_ast: options.dump_ast,
      dump_bytecode: options.dump_bytecode,
   });
   mica::std::load(&engine)?;
   Ok(engine)
}

fn repl(engine_options: &EngineOptions) -> Result<(), mica::Error> {
   println!("Mica {} REPL", env!("CARGO_PKG_VERSION"));
   println!("Press Ctrl-C to exit.");
   println!();

   let mut editor =
      Editor::with_config(rustyline::Config::builder().auto_add_history(true).build());
   editor.set_helper(Some(MicaValidator));

   let engine = engine(engine_options)?;
   while let Ok(line) = editor.readline("> ") {
      let iterator = match interpret(&engine, "(repl)", line) {
         Ok(iterator) => iterator,
         Err(_) => break,
      };
      for result in iterator.flatten() {
         println!("< {result:?}");
         println!();
      }
   }

   Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
   let opts = Options::from_args();
   if let Some(path) = &opts.file {
      #[derive(Debug)]
      struct CompileError;
      #[derive(Debug)]
      struct RuntimeError;

      impl fmt::Display for CompileError {
         fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(self, f)
         }
      }
      impl fmt::Display for RuntimeError {
         fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(self, f)
         }
      }

      impl std::error::Error for CompileError {}
      impl std::error::Error for RuntimeError {}

      let file = std::fs::read_to_string(path)?;
      let engine = engine(&opts.engine_options)?;
      for result in interpret(&engine, path.to_str().unwrap(), file).map_err(|_| CompileError)? {
         result.map_err(|_| RuntimeError)?;
      }
   } else {
      repl(&opts.engine_options)?;
   }

   Ok(())
}
