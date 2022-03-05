#![allow(clippy::vec_box)]

use std::path::PathBuf;

use bytecode::{Chunk, GlobalInfo};
use codegen::CodeGenerator;
use common::{Error, ErrorKind};
use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Helper};

use lexer::Lexer;
use parser::Parser;
use structopt::StructOpt;
use value::Value;
use vm::{Globals, Vm};

mod ast;
mod bytecode;
mod codegen;
mod common;
mod lexer;
mod parser;
mod value;
mod vm;

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
      let lexer = Lexer::new(ctx.input().to_owned());
      let parser = Parser::new(lexer);
      if let Err(error) = parser.parse() {
         match error.kind {
            | ErrorKind::MissingClosingQuote
            | ErrorKind::MissingEnd
            | ErrorKind::MissingRightParen => return Ok(ValidationResult::Incomplete),
            _ => (),
         }
      }
      Ok(ValidationResult::Valid(None))
   }
}

fn compile(globals: &mut GlobalInfo, input: String) -> Result<Chunk, Error> {
   let lexer = Lexer::new(input);
   let parser = Parser::new(lexer);
   let (ast, root_node) = parser.parse()?;
   CodeGenerator::new(globals).generate(&ast, root_node)
}

fn interpret(
   vm: &mut Vm,
   (globals, global_info): (&mut Globals, &mut GlobalInfo),
   input: String,
) -> Option<Value> {
   let chunk = match compile(global_info, input) {
      Ok(chunk) => chunk,
      Err(error) => {
         eprintln!("{error}");
         return None;
      }
   };
   // println!("{global_info:?}");
   // println!("{chunk:?}");
   let result = match vm.interpret(&chunk, globals) {
      Ok(value) => value,
      Err(error) => {
         eprintln!("{error}");
         return None;
      }
   };
   Some(result)
}

fn repl() {
   let mut editor =
      Editor::with_config(rustyline::Config::builder().auto_add_history(true).build());
   editor.set_helper(Some(MicaValidator));
   let mut vm = Vm::new();
   let mut globals = Globals::new();
   let mut global_info = GlobalInfo::new();
   while let Ok(line) = editor.readline("> ") {
      if let Some(result) = interpret(&mut vm, (&mut globals, &mut global_info), line) {
         println!("< {result:?}");
         println!();
      }
   }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
   let opts = Opts::from_args();
   if let Some(path) = &opts.file {
      let file = std::fs::read_to_string(path)?;
      if let Some(value) = interpret(
         &mut Vm::new(),
         (&mut Globals::new(), &mut GlobalInfo::new()),
         file,
      ) {
         println!("{value:#?}");
      }
   } else {
      repl();
   }

   Ok(())
}
