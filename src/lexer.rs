use crate::common::{Error, ErrorKind, Location};

#[derive(Debug)]
pub enum TokenKind {
   Number(f64),

   Plus,       // +
   Minus,      // -
   Star,       // *
   Slash,      // /
   LeftParen,  // (
   RightParen, // )

   Eof,
}

#[derive(Debug)]
pub struct Token {
   pub kind: TokenKind,
   pub location: Location,
}

pub struct Lexer {
   input: String,
   location: Location,
   token_start: Location,
}

impl Lexer {
   const EOF: char = '\0';

   pub fn new(input: String) -> Self {
      Self {
         input,
         location: Default::default(),
         token_start: Default::default(),
      }
   }

   fn error(&self, kind: ErrorKind) -> Error {
      Error {
         kind,
         location: self.location,
      }
   }

   fn token(&self, kind: TokenKind) -> Token {
      Token {
         kind,
         location: self.token_start,
      }
   }

   fn get(&self) -> char {
      self.input[self.location.byte..].chars().next().unwrap_or(Self::EOF)
   }

   fn advance(&mut self) {
      self.location.byte += self.get().len_utf8();
      self.location.column += 1;
   }

   fn skip_whitespace(&mut self) {
      loop {
         match self.get() {
            ' ' | '\t' => {
               self.advance();
            }
            '\n' => {
               self.advance();
               self.location.line += 1;
               self.location.column = 1;
            }
            _ => break,
         }
      }
   }

   fn number(&mut self) -> Result<f64, Error> {
      let start = self.location.byte;
      while let '0'..='9' = self.get() {
         self.advance();
      }
      if self.get() == '.' {
         self.advance();
         if !matches!(self.get(), '0'..='9') {
            return Err(self.error(ErrorKind::MissingDigitsAfterDecimalPoint));
         }
      }
      let end = self.location.byte;

      // Parsing here must succeed as we only allow decimal digits and a decimal point '.'.
      let number = self.input[start..end].parse().unwrap();
      Ok(number)
   }

   fn single_char_token(&mut self, kind: TokenKind) -> Token {
      self.advance();
      self.token(kind)
   }

   pub fn next(&mut self) -> Result<Token, Error> {
      self.skip_whitespace();
      self.token_start = self.location;

      match self.get() {
         '0'..='9' => {
            let number = self.number()?;
            Ok(self.token(TokenKind::Number(number)))
         }
         '+' => Ok(self.single_char_token(TokenKind::Plus)),
         '-' => Ok(self.single_char_token(TokenKind::Minus)),
         '*' => Ok(self.single_char_token(TokenKind::Star)),
         '/' => Ok(self.single_char_token(TokenKind::Slash)),
         '(' => Ok(self.single_char_token(TokenKind::LeftParen)),
         ')' => Ok(self.single_char_token(TokenKind::RightParen)),
         Self::EOF => Ok(self.token(TokenKind::Eof)),
         other => Err(self.error(ErrorKind::InvalidCharacter(other))),
      }
   }

   pub fn peek(&mut self) -> Result<Token, Error> {
      let location = self.location;
      let token = self.next()?;
      self.location = location;
      Ok(token)
   }
}
