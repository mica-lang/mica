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
   InvalidCharacter(char),
   MissingDigitsAfterDecimalPoint,
   InvalidPrefixToken,
   InvalidInfixToken,
   UnexpectedTokensAfterEof,
   MissingRightParen,
}

impl std::fmt::Display for ErrorKind {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
         Self::InvalidCharacter(c) => write!(f, "invalid character: {:?}", c),
         Self::MissingDigitsAfterDecimalPoint => write!(f, "missing digits after decimal point"),
         Self::InvalidPrefixToken => write!(f, "invalid token in prefix position"),
         Self::InvalidInfixToken => write!(f, "invalid token in infix position"),
         Self::UnexpectedTokensAfterEof => write!(f, "unexpected tokens at end of input"),
         Self::MissingRightParen => write!(f, "missing right parenthesis ')'"),
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
