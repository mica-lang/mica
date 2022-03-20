//! The lexer.

use std::rc::Rc;

use crate::common::{Error, ErrorKind, Location};

/// The kind of a token.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
   Number(f64),
   String(Rc<str>),

   Identifier(Rc<str>),

   Nil,
   True,
   False,

   Do,
   If,
   Elif,
   Else,
   While,
   Func,
   End,
   Break,
   Return,

   Struct,
   Impl,
   Constructor,
   Static,

   Plus,  // +
   Minus, // -
   Star,  // *
   Slash, // /

   Bang,         // !
   And,          // and
   Or,           // or
   Equal,        // ==
   NotEqual,     // !=
   Less,         // <
   Greater,      // >
   LessEqual,    // <=
   GreaterEqual, // >=

   Assign, // =
   Dot,    // .
   At,     // @

   LeftParen,  // (
   RightParen, // )
   Comma,      // ,

   Eof,
}

/// A token kind paired with its source code location.
#[derive(Debug, Clone)]
pub struct Token {
   pub kind: TokenKind,
   pub location: Location,
}

/// Lexer state.
pub struct Lexer {
   pub module_name: Rc<str>,
   input: String,
   location: Location,
   token_start: Location,
}

impl Lexer {
   /// The EOF sentinel character.
   const EOF: char = '\0';

   /// Creates a new lexer.
   pub fn new(module_name: Rc<str>, input: String) -> Self {
      Self {
         module_name,
         input,
         location: Default::default(),
         token_start: Default::default(),
      }
   }

   /// Emits an error.
   fn error(&self, kind: ErrorKind) -> Error {
      Error::Compile {
         module_name: Rc::clone(&self.module_name),
         kind,
         location: self.location,
      }
   }

   /// Emits a token at the `token_start` location.
   fn token(&self, kind: TokenKind) -> Token {
      Token {
         kind,
         location: self.token_start,
      }
   }

   /// Returns the character at the current position.
   fn get(&self) -> char {
      self.input[self.location.byte..].chars().next().unwrap_or(Self::EOF)
   }

   /// Advances the current position by a character.
   fn advance(&mut self) {
      self.location.byte += self.get().len_utf8();
      self.location.column += 1;
   }

   /// Advances the source location to the next line.
   fn advance_line(&mut self) {
      self.location.line += 1;
      self.location.column = 1;
   }

   /// Skips whitespace characters.
   fn skip_whitespace(&mut self) {
      loop {
         match self.get() {
            ' ' | '\t' => {
               self.advance();
            }
            '#' => {
               while self.get() != '\n' {
                  self.advance();
               }
            }
            '\n' => {
               self.advance();
               self.advance_line();
            }
            _ => break,
         }
      }
   }

   /// Parses a number.
   fn number(&mut self) -> Result<f64, Error> {
      let start = self.location.byte;
      while let '0'..='9' = self.get() {
         self.advance();
      }
      if self.get() == '.' {
         let dot = self.location.byte;
         self.advance();
         if Self::is_identifier_start_char(self.get()) {
            self.location.byte = dot;
         } else if let '0'..='9' = self.get() {
            while let '0'..='9' = self.get() {
               self.advance();
            }
         } else {
            return Err(self.error(ErrorKind::MissingDigitsAfterDecimalPoint));
         }
      }
      let end = self.location.byte;

      // Parsing here must succeed as we only allow decimal digits and a decimal point '.'.
      let number = self.input[start..end].parse().unwrap();
      Ok(number)
   }

   /// Parses a string.
   fn string(&mut self) -> Result<String, Error> {
      self.advance();
      let mut result = String::new();
      while self.get() != '"' {
         if self.get() == Self::EOF {
            return Err(self.error(ErrorKind::MissingClosingQuote));
         }
         if self.get() == '\n' {
            self.advance_line();
         }
         result.push(match self.get() {
            '\\' => {
               self.advance();
               match self.get() {
                  '"' => '"',
                  '\\' => '\\',
                  other => return Err(self.error(ErrorKind::InvalidEscape(other))),
               }
            }
            other => other,
         });
         self.advance();
      }
      self.advance();
      Ok(result)
   }

   /// Parses a single character token.
   fn single_char_token(&mut self, kind: TokenKind) -> Token {
      self.advance();
      self.token(kind)
   }

   /// Parses a token that's either one or two characters.
   fn single_or_double_char_token(
      &mut self,
      single: TokenKind,
      second: char,
      double: TokenKind,
   ) -> Token {
      self.advance();
      if self.get() == second {
         self.advance();
         self.token(double)
      } else {
         self.token(single)
      }
   }

   /// Returns whether `c` can be the first character of an identifier.
   fn is_identifier_start_char(c: char) -> bool {
      c.is_alphabetic() || c == '_'
   }

   /// Returns whether `c` can be a continuing character of an identifier.
   fn is_identifier_char(c: char) -> bool {
      c.is_alphanumeric() || c == '_'
   }

   /// Parses an identifier.
   fn identifier(&mut self) -> &str {
      let start = self.location.byte;
      while Self::is_identifier_char(self.get()) {
         self.advance();
      }
      let end = self.location.byte;
      &self.input[start..end]
   }

   /// Returns which keyword this identifier corresponds to, or `None` if the identifier is not
   /// reserved.
   fn keyword(identifier: &str) -> Option<TokenKind> {
      Some(match identifier {
         "nil" => TokenKind::Nil,
         "true" => TokenKind::True,
         "false" => TokenKind::False,

         "and" => TokenKind::And,
         "or" => TokenKind::Or,

         "do" => TokenKind::Do,
         "if" => TokenKind::If,
         "elif" => TokenKind::Elif,
         "else" => TokenKind::Else,
         "while" => TokenKind::While,
         "func" => TokenKind::Func,
         "end" => TokenKind::End,
         "break" => TokenKind::Break,
         "return" => TokenKind::Return,

         "struct" => TokenKind::Struct,
         "impl" => TokenKind::Impl,
         "constructor" => TokenKind::Constructor,
         "static" => TokenKind::Static,

         _ => return None,
      })
   }

   /// Parses the next token and returns it.
   pub fn next_token(&mut self) -> Result<Token, Error> {
      self.skip_whitespace();
      self.token_start = self.location;

      match self.get() {
         '0'..='9' => {
            let number = self.number()?;
            Ok(self.token(TokenKind::Number(number)))
         }
         '"' => {
            let string = self.string()?;
            Ok(self.token(TokenKind::String(Rc::from(string))))
         }

         c if Self::is_identifier_start_char(c) => {
            let identifier = self.identifier();
            Ok(if let Some(keyword) = Self::keyword(identifier) {
               self.token(keyword)
            } else {
               let identifier = Rc::from(identifier);
               self.token(TokenKind::Identifier(identifier))
            })
         }

         '+' => Ok(self.single_char_token(TokenKind::Plus)),
         '-' => Ok(self.single_char_token(TokenKind::Minus)),
         '*' => Ok(self.single_char_token(TokenKind::Star)),
         '/' => Ok(self.single_char_token(TokenKind::Slash)),

         '=' => Ok(self.single_or_double_char_token(TokenKind::Assign, '=', TokenKind::Equal)),
         '!' => Ok(self.single_or_double_char_token(TokenKind::Bang, '=', TokenKind::NotEqual)),
         '<' => Ok(self.single_or_double_char_token(TokenKind::Less, '=', TokenKind::LessEqual)),
         '>' => {
            Ok(self.single_or_double_char_token(TokenKind::Greater, '=', TokenKind::GreaterEqual))
         }

         '.' => Ok(self.single_char_token(TokenKind::Dot)),
         '@' => Ok(self.single_char_token(TokenKind::At)),

         '(' => Ok(self.single_char_token(TokenKind::LeftParen)),
         ')' => Ok(self.single_char_token(TokenKind::RightParen)),
         ',' => Ok(self.single_char_token(TokenKind::Comma)),
         Self::EOF => Ok(self.token(TokenKind::Eof)),
         other => Err(self.error(ErrorKind::InvalidCharacter(other))),
      }
   }

   /// Peeks at what the next token's going to be without advancing the lexer's position.
   pub fn peek_token(&mut self) -> Result<Token, Error> {
      let location = self.location;
      let token = self.next_token()?;
      self.location = location;
      Ok(token)
   }
}
