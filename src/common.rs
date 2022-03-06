use std::rc::Rc;

use crate::value::Type;

#[derive(Debug, Clone, Copy)]
pub struct Location {
   pub byte: usize,
   pub line: u32,
   pub column: u32,
}

impl Location {
   pub const UNINIT: Self = Self {
      byte: 0,
      line: 0,
      column: 0,
   };
}

impl Default for Location {
   fn default() -> Self {
      Self {
         byte: 0,
         line: 1,
         column: 1,
      }
   }
}

impl std::fmt::Display for Location {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}:{}", self.line, self.column)
   }
}

#[derive(Debug)]
pub enum ErrorKind {
   // Lexer
   InvalidCharacter(char),
   MissingDigitsAfterDecimalPoint,
   MissingClosingQuote,
   InvalidEscape(char),

   // Parser
   InvalidPrefixToken,
   InvalidInfixToken,
   MissingDo,
   MissingRightParen,
   MissingEnd,
   InvalidIfBranchToken,
   BranchAfterElse,
   IdentifierExpected,
   LeftParenExpected,
   UnexpectedEof,
   CommaExpected,

   // Code generator
   VariableDoesNotExist(String),
   TooManyLocals,
   TooManyGlobals,
   IfBranchTooLarge,
   IfExpressionTooLarge,
   OperatorRhsTooLarge,
   LoopTooLarge,
   BreakOutsideOfLoop,
   TooManyFunctions,
   TooManyArguments,
   TooManyParameters,

   // Runtime
   TypeError { expected: Type, got: Type },
   InvalidAssignment,
   CannotCall(Type),
}

impl std::fmt::Display for ErrorKind {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Self::InvalidCharacter(c) => write!(f, "invalid character: {c:?}"),
         Self::MissingDigitsAfterDecimalPoint => write!(f, "missing digits after decimal point"),
         Self::MissingClosingQuote => write!(f, "missing closing quote '\"'"),
         Self::InvalidEscape(c) => write!(f, "invalid escape: {c:?}"),

         Self::InvalidPrefixToken => write!(f, "invalid token in prefix position"),
         Self::InvalidInfixToken => write!(f, "invalid token in infix position"),
         Self::MissingDo => write!(f, "'do' expected"),
         Self::MissingRightParen => write!(f, "missing right parenthesis ')'"),
         Self::MissingEnd => write!(f, "missing 'end'"),
         Self::InvalidIfBranchToken => write!(f, "'elif', 'else', or 'end' expected"),
         Self::BranchAfterElse => write!(f, "extraneous branch after 'else'"),
         Self::IdentifierExpected => write!(f, "identifier expected"),
         Self::LeftParenExpected => write!(f, "left parenthesis '(' expected"),
         Self::UnexpectedEof => write!(f, "unexpected end of file"),
         Self::CommaExpected => write!(f, "comma ',' expected"),

         Self::VariableDoesNotExist(name) => write!(f, "variable '{name}' does not exist"),
         Self::TooManyLocals => write!(f, "too many local variables"),
         Self::TooManyGlobals => write!(f, "too many global variables"),
         Self::IfBranchTooLarge => write!(f, "'if' branch is too large"),
         Self::IfExpressionTooLarge => write!(f, "'if' expression is too large"),
         Self::OperatorRhsTooLarge => write!(f, "the right-hand side of the operator is too large"),
         Self::LoopTooLarge => write!(f, "loop is too large"),
         Self::BreakOutsideOfLoop => write!(f, "'break' cannot be used outside of a loop"),
         Self::TooManyFunctions => write!(f, "too many unique functions"),
         Self::TooManyArguments => write!(f, "too many arguments"),
         Self::TooManyParameters => write!(f, "too many parameters"),

         Self::TypeError { expected, got } => {
            write!(f, "type mismatch, expected {expected} but got {got}")
         }
         Self::InvalidAssignment => write!(f, "invalid left hand side of assignment"),
         Self::CannotCall(typ) => write!(f, "{typ} values cannot be called"),
      }
   }
}

#[derive(Debug)]
pub struct StackTraceEntry {
   pub function_name: Rc<str>,
   pub module_name: Rc<str>,
   pub location: Location,
}

#[derive(Debug)]
pub enum Error {
   Compile {
      kind: ErrorKind,
      module_name: Rc<str>,
      location: Location,
   },
   Runtime {
      kind: ErrorKind,
      call_stack: Vec<StackTraceEntry>,
   },
}

impl std::fmt::Display for Error {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      struct FileLocation<'a>(&'a str, Location);

      impl std::fmt::Display for FileLocation<'_> {
         fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let Self(file, location) = self;
            write!(f, "{}:{}", file, location)
         }
      }

      match self {
         Error::Compile {
            kind,
            module_name,
            location,
         } => {
            write!(f, "{}:{}: error: {}", module_name, location, kind)
         }
         Error::Runtime { kind, call_stack } => {
            write!(f, "error: {}", kind);
            let file_location_width = call_stack
               .iter()
               .map(|entry| format!("{}", FileLocation(&entry.module_name, entry.location)).len())
               .max()
               .unwrap();
            for entry in call_stack {
               write!(
                  f,
                  "    {:width$}  {}",
                  FileLocation(&entry.module_name, entry.location),
                  entry.function_name,
                  width = file_location_width,
               )?;
            }
            Ok(())
         }
      }
   }
}
