use rustyline::Editor;

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

fn interpret(input: String) -> Option<Value> {
   let lexer = Lexer::new(input);
   let parser = Parser::new(lexer);
   let root_node = match parser.parse() {
      Ok(node) => node,
      Err(error) => {
         eprintln!("{error}");
         return None;
      }
   };

   let mut interpreter = Interpreter::new();
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
   let mut editor = Editor::<()>::new();
   while let Ok(line) = editor.readline("> ") {
      if let Some(result) = interpret(line) {
         println!("< {result:?}");
      }
   }
}
