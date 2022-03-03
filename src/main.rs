#![allow(clippy::vec_box)]

use ast::Node;
use common::{Error, ErrorKind};
use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Helper};

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use value::Value;

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;
mod value;

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
      let input = ctx.input();
      match parse(input.to_owned()) {
         Ok(_) => Ok(ValidationResult::Valid(None)),
         Err(error) => match &error.kind {
            | ErrorKind::MissingClosingQuote
            | ErrorKind::MissingRightParen
            | ErrorKind::MissingEnd => Ok(ValidationResult::Incomplete),
            _ => Ok(ValidationResult::Invalid(Some(error.to_string()))),
         },
      }
   }
}

fn parse(input: String) -> Result<Box<Node>, Error> {
   let lexer = Lexer::new(input);
   let parser = Parser::new(lexer);
   parser.parse()
}

fn interpret(interpreter: &mut Interpreter, input: String) -> Option<Value> {
   let root_node = match parse(input) {
      Ok(node) => node,
      Err(error) => {
         eprintln!("{error}");
         return None;
      }
   };
   // root_node.dump_to_stdout();
   let result = match interpreter.interpret(&root_node) {
      Ok(value) => value,
      Err(error) => {
         eprintln!("{error}");
         return None;
      }
   };
   Some(result)
}

fn main() {
   let mut editor =
      Editor::with_config(rustyline::Config::builder().auto_add_history(true).build());
   editor.set_helper(Some(MicaValidator));
   let mut interpreter = Interpreter::new();
   while let Ok(line) = editor.readline("> ") {
      if let Some(result) = interpret(&mut interpreter, line) {
         println!("< {result:?}");
         println!();
      }
   }
}
