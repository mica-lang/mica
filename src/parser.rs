use crate::ast::{Node, NodeKind};
use crate::common::{Error, ErrorKind};
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser {
   lexer: Lexer,
}

impl Parser {
   pub fn new(lexer: Lexer) -> Self {
      Self { lexer }
   }

   fn error(token: &Token, kind: ErrorKind) -> Error {
      Error {
         kind,
         location: token.location,
      }
   }

   fn precedence(token: &Token) -> i8 {
      match token.kind {
         | TokenKind::Equal
         | TokenKind::NotEqual
         | TokenKind::Less
         | TokenKind::Greater
         | TokenKind::LessEqual
         | TokenKind::GreaterEqual => 5,
         TokenKind::Plus | TokenKind::Minus => 10,
         TokenKind::Star | TokenKind::Slash => 20,
         _ => 0,
      }
   }

   fn parse_number(&mut self, token: Token) -> Result<Box<Node>, Error> {
      if let TokenKind::Number(x) = &token.kind {
         Ok(Box::new(Node::new(token.location, NodeKind::Number(*x))))
      } else {
         panic!("next token must be a number");
      }
   }

   fn parse_string(&mut self, token: Token) -> Result<Box<Node>, Error> {
      if let TokenKind::String(s) = token.kind {
         Ok(Box::new(Node::new(token.location, NodeKind::String(s))))
      } else {
         panic!("next token must be a string");
      }
   }

   fn unary_operator(
      &mut self,
      token: Token,
      kind: impl FnOnce(Box<Node>) -> NodeKind,
   ) -> Result<Box<Node>, Error> {
      let next_token = self.lexer.next()?;
      Ok(Box::new(Node::new(
         token.location,
         kind(self.parse_prefix(next_token)?),
      )))
   }

   fn parse_prefix(&mut self, token: Token) -> Result<Box<Node>, Error> {
      match &token.kind {
         TokenKind::Number(_) => self.parse_number(token),
         TokenKind::String(_) => self.parse_string(token),
         TokenKind::Minus => self.unary_operator(token, NodeKind::Negate),
         TokenKind::Bang => self.unary_operator(token, NodeKind::Not),
         TokenKind::LeftParen => {
            let inner = self.parse_expression(0)?;
            if !matches!(self.lexer.next()?.kind, TokenKind::RightParen) {
               return Err(Self::error(&token, ErrorKind::MissingRightParen));
            }
            Ok(inner)
         }
         _ => Err(Self::error(&token, ErrorKind::InvalidPrefixToken)),
      }
   }

   fn binary_operator(
      &mut self,
      left: Box<Node>,
      token: Token,
      kind: impl FnOnce(Box<Node>, Box<Node>) -> NodeKind,
   ) -> Result<Box<Node>, Error> {
      Ok(Box::new(Node::new(
         token.location,
         kind(left, self.parse_expression(Self::precedence(&token))?),
      )))
   }

   fn parse_infix(&mut self, left: Box<Node>, token: Token) -> Result<Box<Node>, Error> {
      match &token.kind {
         TokenKind::Plus => self.binary_operator(left, token, NodeKind::Add),
         TokenKind::Minus => self.binary_operator(left, token, NodeKind::Subtract),
         TokenKind::Star => self.binary_operator(left, token, NodeKind::Multiply),
         TokenKind::Slash => self.binary_operator(left, token, NodeKind::Divide),

         TokenKind::Equal => self.binary_operator(left, token, NodeKind::Equal),
         TokenKind::NotEqual => self.binary_operator(left, token, NodeKind::NotEqual),
         TokenKind::Less => self.binary_operator(left, token, NodeKind::Less),
         TokenKind::Greater => self.binary_operator(left, token, NodeKind::Greater),
         TokenKind::LessEqual => self.binary_operator(left, token, NodeKind::LessEqual),
         TokenKind::GreaterEqual => self.binary_operator(left, token, NodeKind::GreaterEqual),

         _ => Err(Self::error(&token, ErrorKind::InvalidInfixToken)),
      }
   }

   fn parse_expression(&mut self, precedence: i8) -> Result<Box<Node>, Error> {
      let mut token = self.lexer.next()?;
      let mut left = self.parse_prefix(token)?;

      while precedence < Self::precedence(&self.lexer.peek()?) {
         token = self.lexer.next()?;
         left = self.parse_infix(left, token)?;
      }

      Ok(left)
   }

   pub fn parse(mut self) -> Result<Box<Node>, Error> {
      let expression = self.parse_expression(0)?;
      let eof = self.lexer.next()?;
      if !matches!(eof.kind, TokenKind::Eof) {
         Err(Self::error(&eof, ErrorKind::UnexpectedTokensAfterEof))
      } else {
         Ok(expression)
      }
   }
}
