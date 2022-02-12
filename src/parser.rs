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
         TokenKind::Plus => 10,
         TokenKind::Minus => 10,
         TokenKind::Star => 20,
         TokenKind::Slash => 20,
         _ => 0,
      }
   }

   fn parse_number(&mut self, number: Token) -> Result<Box<Node>, Error> {
      if let TokenKind::Number(x) = &number.kind {
         Ok(Box::new(Node::new(number.location, NodeKind::Number(*x))))
      } else {
         panic!("next token must be a number");
      }
   }

   fn parse_prefix(&mut self, token: Token) -> Result<Box<Node>, Error> {
      match &token.kind {
         TokenKind::Number(_) => self.parse_number(token),
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

   fn parse_infix(&mut self, left: Box<Node>, token: Token) -> Result<Box<Node>, Error> {
      Ok(Box::new(match &token.kind {
         TokenKind::Plus => Node::new(
            token.location,
            NodeKind::Add(left, self.parse_expression(Self::precedence(&token))?),
         ),
         TokenKind::Minus => Node::new(
            token.location,
            NodeKind::Subtract(left, self.parse_expression(Self::precedence(&token))?),
         ),
         TokenKind::Star => Node::new(
            token.location,
            NodeKind::Multiply(left, self.parse_expression(Self::precedence(&token))?),
         ),
         TokenKind::Slash => Node::new(
            token.location,
            NodeKind::Divide(left, self.parse_expression(Self::precedence(&token))?),
         ),
         _ => return Err(Self::error(&token, ErrorKind::InvalidInfixToken)),
      }))
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
