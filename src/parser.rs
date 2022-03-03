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

   fn expect(
      &mut self,
      kind: TokenKind,
      error: impl FnOnce(Token) -> Error,
   ) -> Result<Token, Error> {
      let next_token = self.lexer.peek()?;
      if next_token.kind == kind {
         Ok(self.lexer.next()?)
      } else {
         Err(error(next_token))
      }
   }

   fn precedence(token: &Token) -> i8 {
      match token.kind {
         TokenKind::Or => 1,
         TokenKind::And => 2,
         TokenKind::Assign => 3,
         | TokenKind::Equal
         | TokenKind::NotEqual
         | TokenKind::Less
         | TokenKind::Greater
         | TokenKind::LessEqual
         | TokenKind::GreaterEqual => 4,
         TokenKind::Plus | TokenKind::Minus => 5,
         TokenKind::Star | TokenKind::Slash => 6,
         _ => 0,
      }
   }

   fn associativity(token: &Token) -> Associativity {
      match token.kind {
         TokenKind::Assign => Associativity::Right,
         _ => Associativity::Left,
      }
   }

   fn parse_unit(&mut self, token: Token, kind: NodeKind) -> Result<Box<Node>, Error> {
      Ok(Box::new(Node::new(token.location, kind)))
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

   fn parse_identifier(&mut self, token: Token) -> Result<Box<Node>, Error> {
      if let TokenKind::Identifier(i) = token.kind {
         Ok(Box::new(Node::new(token.location, NodeKind::Identifier(i))))
      } else {
         panic!("next token must be an identifier");
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

   fn parse_terminated_block(
      &mut self,
      token: &Token,
      children: &mut Vec<Box<Node>>,
      is_terminator: impl Fn(&TokenKind) -> bool,
   ) -> Result<(), Error> {
      while !is_terminator(&self.lexer.peek()?.kind) {
         if self.lexer.peek()?.kind == TokenKind::Eof {
            return Err(Self::error(token, ErrorKind::MissingEnd));
         }
         children.push(self.parse_expression(0)?);
      }
      Ok(())
   }

   fn do_block(&mut self, token: Token) -> Result<Box<Node>, Error> {
      let mut children = Vec::new();
      self.parse_terminated_block(&token, &mut children, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next();
      Ok(Box::new(Node::new(token.location, NodeKind::Do(children))))
   }

   fn if_expression(&mut self, if_token: Token) -> Result<Box<Node>, Error> {
      let mut branches = Vec::new();
      let mut else_token = None;

      loop {
         let (condition, do_token) = if let Some(token) = else_token.clone() {
            (None, token)
         } else {
            let condition = self.parse_expression(0)?;
            let do_token = self.expect(TokenKind::Do, |t| Self::error(&t, ErrorKind::MissingDo))?;
            (Some(condition), do_token)
         };
         let mut branch = Vec::new();
         self.parse_terminated_block(&do_token, &mut branch, |k| {
            matches!(k, TokenKind::Elif | TokenKind::Else | TokenKind::End)
         })?;
         branches.push(Box::new(Node::new(
            do_token.location,
            if let Some(condition) = condition {
               NodeKind::IfBranch(condition, branch)
            } else {
               NodeKind::ElseBranch(branch)
            },
         )));

         let next_token = self.lexer.next()?;
         match &next_token.kind {
            TokenKind::Elif => {
               if else_token.is_some() {
                  return Err(Self::error(&next_token, ErrorKind::BranchAfterElse));
               }
            }
            TokenKind::Else => {
               else_token = Some(next_token);
            }
            TokenKind::Eof => return Err(Self::error(&do_token, ErrorKind::MissingEnd)),
            TokenKind::End => break,
            _ => return Err(Self::error(&next_token, ErrorKind::InvalidIfBranchToken)),
         }
      }

      Ok(Box::new(Node::new(
         if_token.location,
         NodeKind::If(branches),
      )))
   }

   fn while_expression(&mut self, token: Token) -> Result<Box<Node>, Error> {
      let condition = self.parse_expression(0)?;
      let do_token = self.expect(TokenKind::Do, |t| Self::error(&t, ErrorKind::MissingDo))?;
      let mut body = Vec::new();
      self.parse_terminated_block(&do_token, &mut body, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next();
      Ok(Box::new(Node::new(
         token.location,
         NodeKind::While(condition, body),
      )))
   }

   fn parse_prefix(&mut self, token: Token) -> Result<Box<Node>, Error> {
      match &token.kind {
         TokenKind::Nil => self.parse_unit(token, NodeKind::Nil),
         TokenKind::False => self.parse_unit(token, NodeKind::False),
         TokenKind::True => self.parse_unit(token, NodeKind::True),
         TokenKind::Number(_) => self.parse_number(token),
         TokenKind::String(_) => self.parse_string(token),
         TokenKind::Identifier(_) => self.parse_identifier(token),

         TokenKind::Minus => self.unary_operator(token, NodeKind::Negate),
         TokenKind::Bang => self.unary_operator(token, NodeKind::Not),

         TokenKind::LeftParen => {
            let inner = self.parse_expression(0)?;
            if !matches!(self.lexer.next()?.kind, TokenKind::RightParen) {
               return Err(Self::error(&token, ErrorKind::MissingRightParen));
            }
            Ok(inner)
         }

         TokenKind::Do => self.do_block(token),
         TokenKind::If => self.if_expression(token),
         TokenKind::While => self.while_expression(token),

         _ => Err(Self::error(&token, ErrorKind::InvalidPrefixToken)),
      }
   }

   fn binary_operator(
      &mut self,
      left: Box<Node>,
      token: Token,
      kind: impl FnOnce(Box<Node>, Box<Node>) -> NodeKind,
   ) -> Result<Box<Node>, Error> {
      let precedence =
         Self::precedence(&token) - (Self::associativity(&token) == Associativity::Right) as i8;
      Ok(Box::new(Node::new(
         token.location,
         kind(left, self.parse_expression(precedence)?),
      )))
   }

   fn parse_infix(&mut self, left: Box<Node>, token: Token) -> Result<Box<Node>, Error> {
      match &token.kind {
         TokenKind::Plus => self.binary_operator(left, token, NodeKind::Add),
         TokenKind::Minus => self.binary_operator(left, token, NodeKind::Subtract),
         TokenKind::Star => self.binary_operator(left, token, NodeKind::Multiply),
         TokenKind::Slash => self.binary_operator(left, token, NodeKind::Divide),

         TokenKind::And => self.binary_operator(left, token, NodeKind::And),
         TokenKind::Or => self.binary_operator(left, token, NodeKind::Or),
         TokenKind::Equal => self.binary_operator(left, token, NodeKind::Equal),
         TokenKind::NotEqual => self.binary_operator(left, token, NodeKind::NotEqual),
         TokenKind::Less => self.binary_operator(left, token, NodeKind::Less),
         TokenKind::Greater => self.binary_operator(left, token, NodeKind::Greater),
         TokenKind::LessEqual => self.binary_operator(left, token, NodeKind::LessEqual),
         TokenKind::GreaterEqual => self.binary_operator(left, token, NodeKind::GreaterEqual),

         TokenKind::Assign => self.binary_operator(left, token, NodeKind::Assign),

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Associativity {
   Left,
   Right,
}
