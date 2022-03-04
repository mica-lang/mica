use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::{Error, ErrorKind};
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser {
   lexer: Lexer,
   ast: Ast,
}

impl Parser {
   pub fn new(lexer: Lexer) -> Self {
      Self {
         lexer,
         ast: Ast::new(),
      }
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
      error: impl FnOnce(&Token) -> ErrorKind,
   ) -> Result<Token, Error> {
      let next_token = self.lexer.peek()?;
      if next_token.kind == kind {
         Ok(self.lexer.next()?)
      } else {
         Err(Self::error(&next_token, error(&next_token)))
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
         TokenKind::LeftParen => 7,
         _ => 0,
      }
   }

   fn associativity(token: &Token) -> Associativity {
      match token.kind {
         TokenKind::Assign => Associativity::Right,
         _ => Associativity::Left,
      }
   }

   fn parse_unit(&mut self, token: Token, kind: NodeKind) -> NodeId {
      self.ast.build_node(kind, ()).with_location(token.location).done()
   }

   fn parse_number(&mut self, token: Token) -> NodeId {
      if let &TokenKind::Number(x) = &token.kind {
         self
            .ast
            .build_node(NodeKind::Number, ())
            .with_location(token.location)
            .with_number(x)
            .done()
      } else {
         panic!("next token must be a number");
      }
   }

   fn parse_string(&mut self, token: Token) -> NodeId {
      if let TokenKind::String(s) = token.kind {
         self
            .ast
            .build_node(NodeKind::String, ())
            .with_location(token.location)
            .with_string(s)
            .done()
      } else {
         panic!("next token must be a string");
      }
   }

   fn parse_identifier(&mut self, token: Token) -> Result<NodeId, Error> {
      if let TokenKind::Identifier(i) = token.kind {
         Ok(self
            .ast
            .build_node(NodeKind::Identifier, ())
            .with_location(token.location)
            .with_string(i)
            .done())
      } else {
         Err(Self::error(&token, ErrorKind::IdentifierExpected))
      }
   }

   fn unary_operator(&mut self, token: Token, kind: NodeKind) -> Result<NodeId, Error> {
      let next_token = self.lexer.next()?;
      let right = self.parse_prefix(next_token)?;
      Ok(self.ast.build_node(kind, right).with_location(token.location).done())
   }

   fn parse_terminated_block(
      &mut self,
      token: &Token,
      children: &mut Vec<NodeId>,
      is_terminator: impl Fn(&TokenKind) -> bool,
   ) -> Result<(), Error> {
      while !is_terminator(&self.lexer.peek()?.kind) {
         if self.lexer.peek()?.kind == TokenKind::Eof {
            return Err(Self::error(token, ErrorKind::MissingEnd));
         }
         children.push(self.parse_item()?);
      }
      Ok(())
   }

   fn parse_comma_separated(
      &mut self,
      dest: &mut Vec<NodeId>,
      end: TokenKind,
      mut next: impl FnMut(&mut Self) -> Result<NodeId, Error>,
   ) -> Result<(), Error> {
      loop {
         let token = self.lexer.peek()?;
         match &token.kind {
            TokenKind::Eof => return Err(Self::error(&token, ErrorKind::UnexpectedEof)),
            kind if *kind == end => {
               return Ok(());
            }
            _ => (),
         }
         dest.push(next(self)?);
         match self.lexer.next()? {
            Token {
               kind: TokenKind::Comma,
               ..
            } => (),
            t if t.kind == end => return Ok(()),
            token => return Err(Self::error(&token, ErrorKind::CommaExpected)),
         }
      }
   }

   fn parse_do_block(&mut self, token: Token) -> Result<NodeId, Error> {
      let mut children = Vec::new();
      self.parse_terminated_block(&token, &mut children, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next();
      Ok(self
         .ast
         .build_node(NodeKind::Do, ())
         .with_location(token.location)
         .with_children(children)
         .done())
   }

   fn parse_if_expression(&mut self, if_token: Token) -> Result<NodeId, Error> {
      let mut branches = Vec::new();
      let mut else_token = None;

      loop {
         let (condition, do_token) = if let Some(token) = else_token.clone() {
            (None, token)
         } else {
            let condition = self.parse_expression(0)?;
            let do_token = self.expect(TokenKind::Do, |_| ErrorKind::MissingDo)?;
            (Some(condition), do_token)
         };
         let mut branch = Vec::new();
         self.parse_terminated_block(&do_token, &mut branch, |k| {
            matches!(k, TokenKind::Elif | TokenKind::Else | TokenKind::End)
         })?;
         branches.push(
            if let Some(condition) = condition {
               self.ast.build_node(NodeKind::IfBranch, condition).with_children(branch)
            } else {
               self.ast.build_node(NodeKind::ElseBranch, ())
            }
            .with_location(do_token.location)
            .done(),
         );

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

      Ok(self
         .ast
         .build_node(NodeKind::If, ())
         .with_location(if_token.location)
         .with_children(branches)
         .done())
   }

   fn parse_while_expression(&mut self, token: Token) -> Result<NodeId, Error> {
      let condition = self.parse_expression(0)?;
      let do_token = self.expect(TokenKind::Do, |_| ErrorKind::MissingDo)?;
      let mut body = Vec::new();
      self.parse_terminated_block(&do_token, &mut body, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next();
      Ok(self
         .ast
         .build_node(NodeKind::While, condition)
         .with_location(token.location)
         .with_children(body)
         .done())
   }

   fn parse_function_declaration(&mut self) -> Result<NodeId, Error> {
      let func = self.lexer.next()?;

      let name = self.lexer.next()?;
      let name = self.parse_identifier(name)?;

      let left_paren = self.expect(TokenKind::LeftParen, |_| ErrorKind::LeftParenExpected)?;
      let mut parameters = Vec::new();
      self.parse_comma_separated(&mut parameters, TokenKind::RightParen, |p| {
         let name = p.lexer.next()?;
         p.parse_identifier(name)
      })?;
      let parameters = self
         .ast
         .build_node(NodeKind::Parameters, ())
         .with_location(left_paren.location)
         .with_children(parameters)
         .done();

      let mut body = Vec::new();
      self.parse_terminated_block(&func, &mut body, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next();

      Ok(self
         .ast
         .build_node(NodeKind::Func, (name, parameters))
         .with_location(func.location)
         .with_children(body)
         .done())
   }

   fn parse_break_like(&mut self, token: Token, kind: NodeKind) -> Result<NodeId, Error> {
      let next_token = self.lexer.peek()?;
      let result = if next_token.location.line > token.location.line
         || matches!(next_token.kind, TokenKind::End)
      {
         NodeId::EMPTY
      } else {
         self.parse_expression(0)?
      };
      Ok(self.ast.build_node(kind, result).with_location(token.location).done())
   }

   fn parse_prefix(&mut self, token: Token) -> Result<NodeId, Error> {
      match &token.kind {
         TokenKind::Nil => Ok(self.parse_unit(token, NodeKind::Nil)),
         TokenKind::False => Ok(self.parse_unit(token, NodeKind::False)),
         TokenKind::True => Ok(self.parse_unit(token, NodeKind::True)),
         TokenKind::Number(_) => Ok(self.parse_number(token)),
         TokenKind::String(_) => Ok(self.parse_string(token)),
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

         TokenKind::Do => self.parse_do_block(token),
         TokenKind::If => self.parse_if_expression(token),
         TokenKind::While => self.parse_while_expression(token),

         TokenKind::Break => self.parse_break_like(token, NodeKind::Break),
         TokenKind::Return => self.parse_break_like(token, NodeKind::Return),

         _ => Err(Self::error(&token, ErrorKind::InvalidPrefixToken)),
      }
   }

   fn binary_operator(
      &mut self,
      left: NodeId,
      token: Token,
      kind: NodeKind,
   ) -> Result<NodeId, Error> {
      let precedence =
         Self::precedence(&token) - (Self::associativity(&token) == Associativity::Right) as i8;
      let right = self.parse_expression(precedence)?;
      Ok(self.ast.build_node(kind, (left, right)).with_location(token.location).done())
   }

   fn function_call(&mut self, left: NodeId, left_paren: Token) -> Result<NodeId, Error> {
      let mut arguments = Vec::new();
      self.parse_comma_separated(&mut arguments, TokenKind::RightParen, |p| {
         p.parse_expression(0)
      })?;
      Ok(self
         .ast
         .build_node(NodeKind::Call, left)
         .with_location(left_paren.location)
         .with_children(arguments)
         .done())
   }

   fn parse_infix(&mut self, left: NodeId, token: Token) -> Result<NodeId, Error> {
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

         TokenKind::LeftParen => self.function_call(left, token),

         _ => Err(Self::error(&token, ErrorKind::InvalidInfixToken)),
      }
   }

   fn parse_expression(&mut self, precedence: i8) -> Result<NodeId, Error> {
      let mut token = self.lexer.next()?;
      let mut left = self.parse_prefix(token)?;

      while precedence < Self::precedence(&self.lexer.peek()?) {
         token = self.lexer.next()?;
         left = self.parse_infix(left, token)?;
      }

      Ok(left)
   }

   fn parse_item(&mut self) -> Result<NodeId, Error> {
      let token = self.lexer.peek()?;
      match &token.kind {
         TokenKind::Func => self.parse_function_declaration(),
         _ => self.parse_expression(0),
      }
   }

   pub fn parse(mut self) -> Result<(Ast, NodeId), Error> {
      let first_token = self.lexer.peek()?;
      let mut main = Vec::new();
      Ok(loop {
         let item = self.parse_item()?;
         main.push(item);
         if self.lexer.peek()?.kind == TokenKind::Eof {
            let main = self
               .ast
               .build_node(NodeKind::Main, ())
               .with_location(first_token.location)
               .with_children(main)
               .done();
            break (self.ast, main);
         }
      })
   }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Associativity {
   Left,
   Right,
}
