//! The parser.

use std::rc::Rc;

use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::{Error, ErrorKind};
use crate::lexer::{Lexer, Token, TokenKind};

/// The parser's state.
pub struct Parser {
   lexer: Lexer,
   ast: Ast,
}

impl Parser {
   /// Constructs a new parser from a lexer.
   pub fn new(lexer: Lexer) -> Self {
      Self {
         ast: Ast::new(Rc::clone(&lexer.module_name)),
         lexer,
      }
   }

   /// Constructs a compilation error located at the given token.
   fn error(&self, token: &Token, kind: ErrorKind) -> Error {
      Error::Compile {
         module_name: Rc::clone(&self.lexer.module_name),
         kind,
         location: token.location,
      }
   }

   /// Returns an error if the next token is not of the given kind.
   fn expect(
      &mut self,
      kind: TokenKind,
      error: impl FnOnce(&Token) -> ErrorKind,
   ) -> Result<Token, Error> {
      let next_token = self.lexer.peek_token()?;
      if next_token.kind == kind {
         Ok(self.lexer.next_token()?)
      } else {
         Err(self.error(&next_token, error(&next_token)))
      }
   }

   /// If the next token's kind is equal to `kind`, advances to the next token and returns the
   /// token. Otherwise returns `None`.
   fn try_next(&mut self, kind: TokenKind) -> Result<Option<Token>, Error> {
      let next_token = self.lexer.peek_token()?;
      Ok(if next_token.kind == kind {
         Some(self.lexer.next_token()?)
      } else {
         None
      })
   }

   /// Returns the precedence level of the given token kind.
   fn precedence(kind: &TokenKind) -> i8 {
      match kind {
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
         TokenKind::LeftParen | TokenKind::Dot => 7,
         _ => 0,
      }
   }

   /// Returns the associativity of the given token kind.
   fn associativity(kind: &TokenKind) -> Associativity {
      match kind {
         TokenKind::Assign => Associativity::Right,
         _ => Associativity::Left,
      }
   }

   /// Parses a "unit literal". This is used for all literals that are uniquely identified by a
   /// single token's kind (such as `nil`.)
   fn parse_unit(&mut self, token: Token, kind: NodeKind) -> NodeId {
      self.ast.build_node(kind, ()).with_location(token.location).done()
   }

   /// Parses a number literal.
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

   /// Parses a string literal.
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

   /// Parses a sequence of long string literals.
   fn parse_long_string(&mut self, first: Token) -> Result<NodeId, Error> {
      let mut content = String::new();
      if let TokenKind::LongString(s) = first.kind {
         content.push_str(&s);
      } else {
         panic!("first token must be a long string")
      }
      while let TokenKind::LongString(_) = self.lexer.peek_token()?.kind {
         let s = match self.lexer.next_token()?.kind {
            TokenKind::LongString(s) => s,
            _ => unreachable!(),
         };
         content.push('\n');
         content.push_str(&s);
      }
      Ok(self
         .ast
         .build_node(NodeKind::String, ())
         .with_location(first.location)
         .with_string(Rc::from(content))
         .done())
   }

   /// Parses an identifier.
   fn parse_identifier(&mut self, token: Token) -> Result<NodeId, Error> {
      if let TokenKind::Identifier(i) = token.kind {
         Ok(self
            .ast
            .build_node(NodeKind::Identifier, ())
            .with_location(token.location)
            .with_string(i)
            .done())
      } else {
         Err(self.error(&token, ErrorKind::IdentifierExpected))
      }
   }

   /// Parses a unary operator.
   fn unary_operator(&mut self, token: Token, kind: NodeKind) -> Result<NodeId, Error> {
      let right = self.parse_expression(Self::precedence(&TokenKind::Star))?;
      Ok(self.ast.build_node(kind, right).with_location(token.location).done())
   }

   /// Parses a terminated block. Typically blocks are terminated with `end`.
   fn parse_terminated_block(
      &mut self,
      token: &Token,
      children: &mut Vec<NodeId>,
      is_terminator: impl Fn(&TokenKind) -> bool,
   ) -> Result<(), Error> {
      while !is_terminator(&self.lexer.peek_token()?.kind) {
         if self.lexer.peek_token()?.kind == TokenKind::Eof {
            return Err(self.error(token, ErrorKind::MissingEnd));
         }
         children.push(self.parse_item()?);
      }
      Ok(())
   }

   /// Parses a comma-separated list.
   fn parse_comma_separated(
      &mut self,
      dest: &mut Vec<NodeId>,
      end: TokenKind,
      mut next: impl FnMut(&mut Self) -> Result<NodeId, Error>,
   ) -> Result<(), Error> {
      loop {
         let token = self.lexer.peek_token()?;
         match &token.kind {
            TokenKind::Eof => return Err(self.error(&token, ErrorKind::UnexpectedEof)),
            kind if *kind == end => {
               self.lexer.next_token()?;
               return Ok(());
            }
            _ => (),
         }
         dest.push(next(self)?);
         match self.lexer.next_token()? {
            Token {
               kind: TokenKind::Comma,
               ..
            } => (),
            t if t.kind == end => return Ok(()),
            token => return Err(self.error(&token, ErrorKind::CommaExpected)),
         }
      }
   }

   /// Parses a `do` block.
   fn parse_do_block(&mut self, token: Token) -> Result<NodeId, Error> {
      let mut children = Vec::new();
      self.parse_terminated_block(&token, &mut children, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next_token();
      Ok(self
         .ast
         .build_node(NodeKind::Do, ())
         .with_location(token.location)
         .with_children(children)
         .done())
   }

   /// Parses an `if` expression.
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
               self.ast.build_node(NodeKind::ElseBranch, ()).with_children(branch)
            }
            .with_location(do_token.location)
            .done(),
         );

         let next_token = self.lexer.next_token()?;
         match &next_token.kind {
            TokenKind::Elif => {
               if else_token.is_some() {
                  return Err(self.error(&next_token, ErrorKind::BranchAfterElse));
               }
            }
            TokenKind::Else => {
               else_token = Some(next_token);
            }
            TokenKind::Eof => return Err(self.error(&do_token, ErrorKind::MissingEnd)),
            TokenKind::End => break,
            _ => return Err(self.error(&next_token, ErrorKind::InvalidIfBranchToken)),
         }
      }

      Ok(self
         .ast
         .build_node(NodeKind::If, ())
         .with_location(if_token.location)
         .with_children(branches)
         .done())
   }

   /// Parses a `while` expression.
   fn parse_while_expression(&mut self, token: Token) -> Result<NodeId, Error> {
      let condition = self.parse_expression(0)?;
      let do_token = self.expect(TokenKind::Do, |_| ErrorKind::MissingDo)?;
      let mut body = Vec::new();
      self.parse_terminated_block(&do_token, &mut body, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next_token();
      Ok(self
         .ast
         .build_node(NodeKind::While, condition)
         .with_location(token.location)
         .with_children(body)
         .done())
   }

   /// Parses a function. `anonymous` decides if the function has a name or not.
   fn parse_function(&mut self, func_token: Token, anonymous: bool) -> Result<NodeId, Error> {
      let name = if !anonymous {
         let name = self.lexer.next_token()?;
         self.parse_identifier(name)?
      } else {
         NodeId::EMPTY
      };

      let left_paren = self.expect(TokenKind::LeftParen, |_| ErrorKind::LeftParenExpected)?;
      let mut parameters = Vec::new();
      self.parse_comma_separated(&mut parameters, TokenKind::RightParen, |p| {
         let name = p.lexer.next_token()?;
         p.parse_identifier(name)
      })?;

      // We allow either `constructor` or `static`, but not both.
      let kind = if let Some(token) = self.try_next(TokenKind::Constructor)? {
         self.ast.build_node(NodeKind::Constructor, ()).with_location(token.location).done()
      } else if let Some(token) = self.try_next(TokenKind::Static)? {
         self.ast.build_node(NodeKind::Static, ()).with_location(token.location).done()
      } else {
         NodeId::EMPTY
      };

      let parameters = self
         .ast
         .build_node(NodeKind::Parameters, kind)
         .with_location(left_paren.location)
         .with_children(parameters)
         .done();

      let mut body = Vec::new();
      self.parse_terminated_block(&func_token, &mut body, |k| *k == TokenKind::End)?;
      let _end = self.lexer.next_token();

      Ok(self
         .ast
         .build_node(NodeKind::Func, (name, parameters))
         .with_location(func_token.location)
         .with_children(body)
         .done())
   }

   /// Parses a "break-like" expression. This includes `break` and `return`.
   ///
   /// A break-like expression is a token followed followed by an optional value on the same line as
   /// that token.
   fn parse_break_like(&mut self, token: Token, kind: NodeKind) -> Result<NodeId, Error> {
      let next_token = self.lexer.peek_token()?;
      let result = if next_token.location.line > token.location.line
         || matches!(next_token.kind, TokenKind::End)
      {
         NodeId::EMPTY
      } else {
         self.parse_expression(0)?
      };
      Ok(self.ast.build_node(kind, result).with_location(token.location).done())
   }

   /// Parses a prefix expression.
   fn parse_prefix(&mut self, token: Token) -> Result<NodeId, Error> {
      match &token.kind {
         TokenKind::Nil => Ok(self.parse_unit(token, NodeKind::Nil)),
         TokenKind::False => Ok(self.parse_unit(token, NodeKind::False)),
         TokenKind::True => Ok(self.parse_unit(token, NodeKind::True)),
         TokenKind::Number(_) => Ok(self.parse_number(token)),
         TokenKind::String(_) => Ok(self.parse_string(token)),
         TokenKind::LongString(_) => self.parse_long_string(token),
         TokenKind::Identifier(_) => self.parse_identifier(token),

         TokenKind::Minus => self.unary_operator(token, NodeKind::Negate),
         TokenKind::Bang => self.unary_operator(token, NodeKind::Not),

         TokenKind::At => {
            let name = self.lexer.next_token()?;
            let name = self.parse_identifier(name)?;
            Ok(self.ast.build_node(NodeKind::Field, name).with_location(token.location).done())
         }

         TokenKind::LeftParen => {
            let inner = self.parse_expression(0)?;
            if !matches!(self.lexer.next_token()?.kind, TokenKind::RightParen) {
               return Err(self.error(&token, ErrorKind::MissingRightParen));
            }
            Ok(inner)
         }

         TokenKind::Do => self.parse_do_block(token),
         TokenKind::If => self.parse_if_expression(token),
         TokenKind::While => self.parse_while_expression(token),

         TokenKind::Break => self.parse_break_like(token, NodeKind::Break),
         TokenKind::Return => self.parse_break_like(token, NodeKind::Return),

         TokenKind::Func => self.parse_function(token, true),

         _ => Err(self.error(&token, ErrorKind::InvalidPrefixToken)),
      }
   }

   /// Parses a binary operator.
   fn binary_operator(
      &mut self,
      left: NodeId,
      token: Token,
      kind: NodeKind,
   ) -> Result<NodeId, Error> {
      let precedence = Self::precedence(&token.kind)
         - (Self::associativity(&token.kind) == Associativity::Right) as i8;
      let right = self.parse_expression(precedence)?;
      Ok(self.ast.build_node(kind, (left, right)).with_location(token.location).done())
   }

   /// Parses a function call.
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

   /// Parses an infix token.
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
         TokenKind::Dot => self.binary_operator(left, token, NodeKind::Dot),

         TokenKind::LeftParen => self.function_call(left, token),

         _ => Err(self.error(&token, ErrorKind::InvalidInfixToken)),
      }
   }

   /// Returns whether an infix token is not allowed to be carried over to the next line.
   fn is_invalid_continuation_token(token: &TokenKind) -> bool {
      matches!(token, TokenKind::LeftParen)
   }

   /// Parses an expression.
   fn parse_expression(&mut self, precedence: i8) -> Result<NodeId, Error> {
      let mut token = self.lexer.next_token()?;
      let mut left = self.parse_prefix(token)?;

      while precedence < Self::precedence(&self.lexer.peek_token()?.kind) {
         let next_token = self.lexer.peek_token()?;
         if Self::is_invalid_continuation_token(&next_token.kind)
            && next_token.location.line > self.ast.location(left).line
         {
            break;
         }
         token = self.lexer.next_token()?;
         left = self.parse_infix(left, token)?;
      }

      Ok(left)
   }

   /// Parses a struct declaration.
   fn parse_struct(&mut self) -> Result<NodeId, Error> {
      let struct_token = self.lexer.next_token()?;
      let name = self.lexer.next_token()?;
      let name = self.parse_identifier(name)?;
      Ok(self.ast.build_node(NodeKind::Struct, name).with_location(struct_token.location).done())
   }

   /// Parses an `impl` block.
   fn parse_impl(&mut self) -> Result<NodeId, Error> {
      let impl_token = self.lexer.next_token()?;
      let implementee = self.parse_expression(0)?;
      let mut items = Vec::new();
      // Note that we parse any type of item inside of the `impl` block.
      // The codegen phase is the thing that ensures the items declared are valid.
      self.parse_terminated_block(&impl_token, &mut items, |k| k == &TokenKind::End)?;
      let _end = self.lexer.next_token()?;
      Ok(self
         .ast
         .build_node(NodeKind::Impl, implementee)
         .with_location(impl_token.location)
         .with_children(items)
         .done())
   }

   /// Parses a single item.
   fn parse_item(&mut self) -> Result<NodeId, Error> {
      let token = self.lexer.peek_token()?;
      match &token.kind {
         TokenKind::Func => {
            let func_token = self.lexer.next_token()?;
            self.parse_function(func_token, false)
         }
         TokenKind::Struct => self.parse_struct(),
         TokenKind::Impl => self.parse_impl(),
         _ => self.parse_expression(0),
      }
   }

   /// Parses a Mica program.
   pub fn parse(mut self) -> Result<(Ast, NodeId), Error> {
      let first_token = self.lexer.peek_token()?;
      let mut main = Vec::new();
      Ok(loop {
         if self.lexer.peek_token()?.kind == TokenKind::Eof {
            let main = self
               .ast
               .build_node(NodeKind::Main, ())
               .with_location(first_token.location)
               .with_children(main)
               .done();
            break (self.ast, main);
         }
         let item = self.parse_item()?;
         main.push(item);
      })
   }
}

/// The associativity of an infix token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Associativity {
   Left,
   Right,
}
