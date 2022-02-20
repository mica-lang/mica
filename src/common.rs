use crate::value::Type;

#[derive(Debug, Clone, Copy)]
pub struct Location {
   pub byte: usize,
   pub line: u32,
   pub column: u32,
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
   CharacterExpected(char),
   MissingDigitsAfterDecimalPoint,
   MissingClosingQuote,
   InvalidEscape(char),

   // Parser
   InvalidPrefixToken,
   InvalidInfixToken,
   UnexpectedTokensAfterEof,
   MissingRightParen,

   // Runtime
   TypeError { expected: Type, got: Type },
}

impl std::fmt::Display for ErrorKind {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Self::InvalidCharacter(c) => write!(f, "invalid character: {c:?}"),
         Self::CharacterExpected(c) => write!(f, "{c:?} expected"),
         Self::MissingDigitsAfterDecimalPoint => write!(f, "missing digits after decimal point"),
         Self::MissingClosingQuote => write!(f, "missing closing quote '\"'"),
         Self::InvalidEscape(c) => write!(f, "invalid escape: {c:?}"),

         Self::InvalidPrefixToken => write!(f, "invalid token in prefix position"),
         Self::InvalidInfixToken => write!(f, "invalid token in infix position"),
         Self::UnexpectedTokensAfterEof => write!(f, "unexpected tokens at end of input"),
         Self::MissingRightParen => write!(f, "missing right parenthesis ')'"),

         Self::TypeError { expected, got } => {
            write!(f, "type mismatch, expected {expected} but got {got}")
         }
      }
   }
}

#[derive(Debug)]
pub struct Error {
   pub kind: ErrorKind,
   pub location: Location,
}

impl std::fmt::Display for Error {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "error at {}: {}", self.location, self.kind)
   }
}
