//! The parser.

use std::{fmt, rc::Rc};

use crate::ll::{
    ast::{Ast, NodeId, NodeKind},
    error::{LanguageError, LanguageErrorKind},
    lexer::{Lexer, Token, TokenKind},
};

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
    fn error(&self, token: &Token, kind: LanguageErrorKind) -> LanguageError {
        LanguageError::Compile {
            module_name: Rc::clone(&self.lexer.module_name),
            kind,
            location: token.location,
        }
    }

    /// Returns an error if the next token is not of the given kind.
    fn expect(
        &mut self,
        kind: TokenKind,
        error: impl FnOnce(&Token) -> LanguageErrorKind,
    ) -> Result<Token, LanguageError> {
        let next_token = self.lexer.peek_token()?;
        if next_token.kind == kind {
            Ok(self.lexer.next_token()?)
        } else {
            Err(self.error(&next_token, error(&next_token)))
        }
    }

    /// If the next token's kind is equal to `kind`, advances to the next token and returns the
    /// token. Otherwise returns `None`.
    fn try_next(&mut self, kind: TokenKind) -> Result<Option<Token>, LanguageError> {
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
            TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::Greater
            | TokenKind::LessEqual
            | TokenKind::GreaterEqual => 4,
            TokenKind::Plus | TokenKind::Minus => 5,
            TokenKind::Star | TokenKind::Slash => 6,
            TokenKind::LeftParen | TokenKind::Dot | TokenKind::Impl => 7,
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
        self.ast
            .build_node(kind, ())
            .with_location(token.location)
            .done()
    }

    /// Parses a number literal.
    fn parse_number(&mut self, token: Token) -> NodeId {
        if let &TokenKind::Number(x) = &token.kind {
            self.ast
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
            self.ast
                .build_node(NodeKind::String, ())
                .with_location(token.location)
                .with_string(s)
                .done()
        } else {
            panic!("next token must be a string");
        }
    }

    /// Parses a sequence of long string literals.
    fn parse_long_string(&mut self, first: Token) -> Result<NodeId, LanguageError> {
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
    fn parse_identifier(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        if let TokenKind::Identifier(i) = token.kind {
            Ok(self
                .ast
                .build_node(NodeKind::Identifier, ())
                .with_location(token.location)
                .with_string(i)
                .done())
        } else {
            Err(self.error(&token, LanguageErrorKind::IdentifierExpected))
        }
    }

    /// Parses a unary operator.
    fn unary_operator(&mut self, token: Token, kind: NodeKind) -> Result<NodeId, LanguageError> {
        let right = self.parse_expression(Self::precedence(&TokenKind::Star))?;
        Ok(self
            .ast
            .build_node(kind, right)
            .with_location(token.location)
            .done())
    }

    /// Parses a terminated block. Typically blocks are terminated with `end`.
    fn parse_terminated_block(
        &mut self,
        token: &Token,
        children: &mut Vec<NodeId>,
        is_terminator: impl Fn(&TokenKind) -> bool,
    ) -> Result<(), LanguageError> {
        while !is_terminator(&self.lexer.peek_token()?.kind) {
            if self.lexer.peek_token()?.kind == TokenKind::Eof {
                return Err(self.error(token, LanguageErrorKind::MissingEnd));
            }
            children.push(self.parse_item()?);
        }
        Ok(())
    }

    /// Parses a comma-separated list, providing a function that returns whether parsing should end.
    fn parse_comma_separated_ending_with(
        &mut self,
        dest: &mut Vec<NodeId>,
        is_end: impl Fn(&TokenKind) -> bool,
        mut next: impl FnMut(&mut Self) -> Result<NodeId, LanguageError>,
    ) -> Result<Token, LanguageError> {
        loop {
            let token = self.lexer.peek_token()?;
            match &token.kind {
                TokenKind::Eof => return Err(self.error(&token, LanguageErrorKind::UnexpectedEof)),
                kind if is_end(kind) => {
                    return self.lexer.next_token();
                }
                _ => (),
            }
            dest.push(next(self)?);
            match self.lexer.next_token()? {
                Token {
                    kind: TokenKind::Comma,
                    ..
                } => (),
                t if is_end(&t.kind) => return Ok(t),
                token => return Err(self.error(&token, LanguageErrorKind::CommaExpected)),
            }
        }
    }

    /// Parses a comma-separated list.
    fn parse_comma_separated(
        &mut self,
        dest: &mut Vec<NodeId>,
        end: TokenKind,
        next: impl FnMut(&mut Self) -> Result<NodeId, LanguageError>,
    ) -> Result<Token, LanguageError> {
        self.parse_comma_separated_ending_with(dest, |k| k == &end, next)
    }

    /// Parses a parenthesized expression `(x)` or a tuple - `()`, `(x,)`, or `(x, y)`.
    fn parse_paren_or_tuple(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        if self.lexer.peek_token()?.kind == TokenKind::RightParen {
            let _right_paren = self.lexer.next_token();
            return Ok(self
                .ast
                .build_node(NodeKind::Tuple, ())
                .with_children(vec![])
                .with_location(token.location)
                .done());
        }
        let inner = self.parse_expression(0)?;
        match self.lexer.next_token()?.kind {
            TokenKind::RightParen => {
                let location = self.ast.location(inner);
                Ok(self
                    .ast
                    .build_node(NodeKind::Paren, inner)
                    .with_location(location)
                    .done())
            }
            TokenKind::Comma => {
                let mut elements = vec![inner];
                self.parse_comma_separated(&mut elements, TokenKind::RightParen, |p| {
                    p.parse_expression(0)
                })?;
                Ok(self
                    .ast
                    .build_node(NodeKind::Tuple, ())
                    .with_children(elements)
                    .with_location(token.location)
                    .done())
            }
            _ => Err(self.error(&token, LanguageErrorKind::MissingRightParen)),
        }
    }

    /// Parses a list or dict literal.
    fn parse_list_or_dict(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Mode {
            Unknown,
            Dict,
            List,
        }

        let mut elements = Vec::new();
        let mut mode = Mode::Unknown;
        if self.lexer.peek_token()?.kind == TokenKind::Colon {
            self.lexer.next_token()?;
            self.expect(TokenKind::RightBracket, |_| {
                LanguageErrorKind::RightBracketExpectedToCloseEmptyDict
            })?;
            mode = Mode::Dict;
        } else {
            self.parse_comma_separated(&mut elements, TokenKind::RightBracket, |p| match mode {
                Mode::Unknown => {
                    let key = p.parse_expression(0)?;
                    if p.lexer.peek_token()?.kind == TokenKind::Colon {
                        mode = Mode::Dict;
                        let colon = p.lexer.next_token()?;
                        let value = p.parse_expression(0)?;
                        Ok(p.ast
                            .build_node(NodeKind::Pair, (key, value))
                            .with_location(colon.location)
                            .done())
                    } else {
                        mode = Mode::List;
                        Ok(key)
                    }
                }
                Mode::Dict => {
                    let key = p.parse_expression(0)?;
                    let colon = p.expect(TokenKind::Colon, |_| {
                        LanguageErrorKind::ColonExpectedAfterDictKey
                    })?;
                    let value = p.parse_expression(0)?;
                    Ok(p.ast
                        .build_node(NodeKind::Pair, (key, value))
                        .with_location(colon.location)
                        .done())
                }
                Mode::List => p.parse_expression(0),
            })?;
        }

        Ok(self
            .ast
            .build_node(
                match mode {
                    Mode::Unknown | Mode::List => NodeKind::List,
                    Mode::Dict => NodeKind::Dict,
                },
                (),
            )
            .with_location(token.location)
            .with_children(elements)
            .done())
    }

    fn parse_record(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        let mut fields = vec![];
        let end_token = self.parse_comma_separated_ending_with(
            &mut fields,
            |k| matches!(k, TokenKind::RightBrace | TokenKind::DotDot),
            |p| {
                let key = p.lexer.next_token()?;
                let key = p.parse_identifier(key)?;
                let value = if let TokenKind::Colon = &p.lexer.peek_token()?.kind {
                    let _colon = p.lexer.next_token()?;
                    p.parse_expression(0)?
                } else {
                    NodeId::EMPTY
                };
                let location = p.ast.location(key);
                Ok(p.ast
                    .build_node(NodeKind::Pair, (key, value))
                    .with_location(location)
                    .done())
            },
        )?;
        if let TokenKind::DotDot = &end_token.kind {
            fields.push(
                self.ast
                    .build_node(NodeKind::Rest, ())
                    .with_location(end_token.location)
                    .done(),
            );
            let _right_brace = self.expect(TokenKind::RightBrace, |_| {
                LanguageErrorKind::RestMustBeFollowedByRightBrace
            })?;
        }
        Ok(self
            .ast
            .build_node(NodeKind::Record, ())
            .with_children(fields)
            .with_location(token.location)
            .done())
    }

    fn parse_let_expression(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        let right = self.parse_expression(0)?;
        Ok(self
            .ast
            .build_node(NodeKind::Let, right)
            .with_location(token.location)
            .done())
    }

    /// Parses a `do` block.
    fn parse_do_block(&mut self, token: Token) -> Result<NodeId, LanguageError> {
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
    fn parse_if_expression(&mut self, if_token: Token) -> Result<NodeId, LanguageError> {
        let mut branches = Vec::new();
        let mut else_token = None;

        loop {
            let (condition, do_token) = if let Some(token) = else_token.clone() {
                (None, token)
            } else {
                let condition = self.parse_expression(0)?;
                let do_token = self.expect(TokenKind::Do, |_| LanguageErrorKind::MissingDo)?;
                (Some(condition), do_token)
            };
            let mut branch = Vec::new();
            self.parse_terminated_block(&do_token, &mut branch, |k| {
                matches!(k, TokenKind::Elif | TokenKind::Else | TokenKind::End)
            })?;
            branches.push(
                if let Some(condition) = condition {
                    self.ast
                        .build_node(NodeKind::IfBranch, condition)
                        .with_children(branch)
                } else {
                    self.ast
                        .build_node(NodeKind::ElseBranch, ())
                        .with_children(branch)
                }
                .with_location(do_token.location)
                .done(),
            );

            let next_token = self.lexer.next_token()?;
            match &next_token.kind {
                TokenKind::Elif => {
                    if else_token.is_some() {
                        return Err(self.error(&next_token, LanguageErrorKind::BranchAfterElse));
                    }
                }
                TokenKind::Else => {
                    else_token = Some(next_token);
                }
                TokenKind::Eof => return Err(self.error(&do_token, LanguageErrorKind::MissingEnd)),
                TokenKind::End => break,
                _ => return Err(self.error(&next_token, LanguageErrorKind::InvalidIfBranchToken)),
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
    fn parse_while_expression(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        let condition = self.parse_expression(0)?;
        let do_token = self.expect(TokenKind::Do, |_| LanguageErrorKind::MissingDo)?;
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

    /// Parses a `for` expression.
    fn parse_for_expression(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        let binding = self.parse_expression(0)?;
        let _in_token = self.expect(TokenKind::In, |_| {
            LanguageErrorKind::InExpectedAfterForBinding
        })?;
        let iterator = self.parse_expression(0)?;
        let do_token = self.expect(TokenKind::Do, |_| LanguageErrorKind::MissingDo)?;
        let mut body = Vec::new();
        self.parse_terminated_block(&do_token, &mut body, |k| *k == TokenKind::End)?;
        let _end = self.lexer.next_token();
        Ok(self
            .ast
            .build_node(NodeKind::For, (binding, iterator))
            .with_location(token.location)
            .with_children(body)
            .done())
    }

    /// Parses a function. `anonymous` decides if the function has a name or not.
    fn parse_function(
        &mut self,
        func_token: Token,
        anonymous: bool,
    ) -> Result<NodeId, LanguageError> {
        let name = if !anonymous {
            let name = self.lexer.next_token()?;
            self.parse_identifier(name)?
        } else {
            NodeId::EMPTY
        };

        let left_paren = self.expect(TokenKind::LeftParen, |_| {
            LanguageErrorKind::LeftParenExpected
        })?;
        let mut parameters = Vec::new();
        self.parse_comma_separated(&mut parameters, TokenKind::RightParen, |p| {
            let name = p.lexer.next_token()?;
            p.parse_identifier(name)
        })?;

        // We allow either `constructor` or `static`, but not both.
        let kind = if let Some(token) = self.try_next(TokenKind::Constructor)? {
            self.ast
                .build_node(NodeKind::Constructor, ())
                .with_location(token.location)
                .done()
        } else if let Some(token) = self.try_next(TokenKind::Static)? {
            self.ast
                .build_node(NodeKind::Static, ())
                .with_location(token.location)
                .done()
        } else {
            NodeId::EMPTY
        };

        let parameters = self
            .ast
            .build_node(NodeKind::Parameters, kind)
            .with_location(left_paren.location)
            .with_children(parameters)
            .done();
        let name_location = self.ast.location(name);
        let head = self
            .ast
            .build_node(NodeKind::FunctionHead, (name, parameters))
            .with_location(name_location)
            .done();

        let body = if self.lexer.peek_token()?.kind == TokenKind::Assign {
            let _equals = self.lexer.next_token();
            self.parse_expression(0)?
        } else {
            NodeId::EMPTY
        };

        Ok(self
            .ast
            .build_node(NodeKind::Func, (head, body))
            .with_location(func_token.location)
            .done())
    }

    /// Parses a "break-like" expression. This includes `break` and `return`.
    ///
    /// A break-like expression is a token followed followed by an optional value on the same line
    /// as that token.
    fn parse_break_like(&mut self, token: Token, kind: NodeKind) -> Result<NodeId, LanguageError> {
        let next_token = self.lexer.peek_token()?;
        let result = if next_token.location.line > token.location.line
            || matches!(next_token.kind, TokenKind::End)
        {
            NodeId::EMPTY
        } else {
            self.parse_expression(0)?
        };
        Ok(self
            .ast
            .build_node(kind, result)
            .with_location(token.location)
            .done())
    }

    /// Parses a struct declaration.
    fn parse_struct(&mut self, struct_token: Token) -> Result<NodeId, LanguageError> {
        let name = self.lexer.next_token()?;
        let name = self.parse_identifier(name)?;
        Ok(self
            .ast
            .build_node(NodeKind::Struct, name)
            .with_location(struct_token.location)
            .done())
    }

    /// Parses an `as` block.
    fn parse_as(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        let implementee = self.parse_expression(0)?;
        let mut items = Vec::new();
        // Note that we parse any type of item inside of the `impl` block.
        // The codegen phase is the thing that ensures the items declared are valid.
        self.parse_terminated_block(&token, &mut items, |k| k == &TokenKind::End)?;
        let _end = self.lexer.next_token()?;
        Ok(self
            .ast
            .build_node(NodeKind::ImplAs, implementee)
            .with_location(token.location)
            .with_children(items)
            .done())
    }

    /// Parses a trait declaration.
    fn parse_trait(&mut self, trait_token: Token) -> Result<NodeId, LanguageError> {
        let name = self.lexer.next_token()?;
        let name = self.parse_identifier(name)?;
        let mut items = Vec::new();
        // Just like with `impl`, we allow for any item here, and the codegen phase ensures they're
        // functions.
        self.parse_terminated_block(&trait_token, &mut items, |k| k == &TokenKind::End)?;
        let _end = self.lexer.next_token()?;
        Ok(self
            .ast
            .build_node(NodeKind::Trait, name)
            .with_location(trait_token.location)
            .with_children(items)
            .done())
    }

    /// Parses a prefix expression.
    fn parse_prefix(&mut self, token: Token) -> Result<NodeId, LanguageError> {
        match &token.kind {
            TokenKind::Nil => Ok(self.parse_unit(token, NodeKind::Nil)),
            TokenKind::False => Ok(self.parse_unit(token, NodeKind::False)),
            TokenKind::True => Ok(self.parse_unit(token, NodeKind::True)),
            TokenKind::Number(_) => Ok(self.parse_number(token)),
            TokenKind::String(_) => Ok(self.parse_string(token)),
            TokenKind::LongString(_) => self.parse_long_string(token),
            TokenKind::Identifier(_) => self.parse_identifier(token),
            TokenKind::Underscore => Ok(self.parse_unit(token, NodeKind::Underscore)),

            TokenKind::Minus => self.unary_operator(token, NodeKind::Negate),
            TokenKind::Bang => self.unary_operator(token, NodeKind::Not),

            TokenKind::At => {
                let name = self.lexer.next_token()?;
                let name = self.parse_identifier(name)?;
                Ok(self
                    .ast
                    .build_node(NodeKind::Field, name)
                    .with_location(token.location)
                    .done())
            }

            TokenKind::LeftParen => self.parse_paren_or_tuple(token),
            TokenKind::LeftBracket => self.parse_list_or_dict(token),
            TokenKind::LeftBrace => self.parse_record(token),

            TokenKind::Let => self.parse_let_expression(token),
            TokenKind::Do => self.parse_do_block(token),
            TokenKind::If => self.parse_if_expression(token),
            TokenKind::While => self.parse_while_expression(token),
            TokenKind::For => self.parse_for_expression(token),

            TokenKind::Break => self.parse_break_like(token, NodeKind::Break),
            TokenKind::Return => self.parse_break_like(token, NodeKind::Return),

            TokenKind::Func => self.parse_function(token, true),
            TokenKind::Struct => self.parse_struct(token),
            TokenKind::As => self.parse_as(token),
            TokenKind::Trait => self.parse_trait(token),

            _ => Err(self.error(&token, LanguageErrorKind::InvalidPrefixToken)),
        }
    }

    /// Parses a binary operator.
    fn binary_operator(
        &mut self,
        left: NodeId,
        token: Token,
        kind: NodeKind,
    ) -> Result<NodeId, LanguageError> {
        let precedence = Self::precedence(&token.kind)
            - (Self::associativity(&token.kind) == Associativity::Right) as i8;
        let right = self.parse_expression(precedence)?;
        Ok(self
            .ast
            .build_node(kind, (left, right))
            .with_location(token.location)
            .done())
    }

    /// Parses a function call.
    fn function_call(&mut self, left: NodeId, left_paren: Token) -> Result<NodeId, LanguageError> {
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

    /// Parses an `impl` block.
    fn parse_impl(&mut self, left: NodeId, token: Token) -> Result<NodeId, LanguageError> {
        let mut items = Vec::new();
        // Note that we parse any type of item inside of the `impl` block.
        // The codegen phase is the thing that ensures the items declared are valid.
        self.parse_terminated_block(&token, &mut items, |k| k == &TokenKind::End)?;
        let _end = self.lexer.next_token()?;
        Ok(self
            .ast
            .build_node(NodeKind::Impl, left)
            .with_location(token.location)
            .with_children(items)
            .done())
    }

    /// Parses an infix token.
    fn parse_infix(&mut self, left: NodeId, token: Token) -> Result<NodeId, LanguageError> {
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

            TokenKind::Impl => self.parse_impl(left, token),

            _ => Err(self.error(&token, LanguageErrorKind::InvalidInfixToken)),
        }
    }

    /// Returns whether an infix token is not allowed to be carried over to the next line.
    fn is_invalid_continuation_token(token: &TokenKind) -> bool {
        matches!(token, TokenKind::LeftParen)
    }

    /// Parses an expression.
    fn parse_expression(&mut self, precedence: i8) -> Result<NodeId, LanguageError> {
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

    /// Parses a single item.
    fn parse_item(&mut self) -> Result<NodeId, LanguageError> {
        let token = self.lexer.peek_token()?;
        match &token.kind {
            TokenKind::Func => {
                let func_token = self.lexer.next_token()?;
                self.parse_function(func_token, false)
            }
            _ => self.parse_expression(0),
        }
    }

    /// Parses a Mica program.
    pub fn parse(mut self) -> Result<(Ast, NodeId), LanguageError> {
        let first_token = self.lexer.peek_token()?;
        let mut main = Vec::new();
        loop {
            if self.lexer.peek_token()?.kind == TokenKind::Eof {
                let main = self
                    .ast
                    .build_node(NodeKind::Main, ())
                    .with_location(first_token.location)
                    .with_children(main)
                    .done();
                return Ok((self.ast, main));
            }
            let item = self.parse_item()?;
            main.push(item);
        }
    }
}

/// The associativity of an infix token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Associativity {
    Left,
    Right,
}

impl fmt::Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser").finish_non_exhaustive()
    }
}
