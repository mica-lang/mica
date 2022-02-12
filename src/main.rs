use rustyline::Editor;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;

fn interpret(input: String) -> Option<f64> {
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
   let result = interpreter.interpret(&root_node);
   Some(result)
}

fn main() {
   let mut editor = Editor::<()>::new();
   while let Ok(line) = editor.readline("> ") {
      if let Some(result) = interpret(line) {
         println!("< {result}");
      }
   }
}
