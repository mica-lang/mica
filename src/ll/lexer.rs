//! The lexer.

use std::rc::Rc;

use crate::ll::error::{Error, ErrorKind, Location};

/// The kind of a token.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(f64),
    String(Rc<str>),
    // NOTE: Long strings \\ are joined into one string literal by the parser.
    LongString(Rc<str>),

    Identifier(Rc<str>),

    Nil,
    True,
    False,

    Do,
    If,
    Elif,
    Else,
    While,
    For,
    In,
    Func,
    End,
    Break,
    Return,

    Struct,
    Trait,
    Impl,
    As,
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
    Colon,  // :
    At,     // @

    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,

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
        Self { module_name, input, location: Default::default(), token_start: Default::default() }
    }

    /// Emits an error.
    fn error(&self, kind: ErrorKind) -> Error {
        self.error_at(self.location, kind)
    }

    /// Emits an error at a specific location.
    fn error_at(&self, location: Location, kind: ErrorKind) -> Error {
        Error::Compile { module_name: Rc::clone(&self.module_name), kind, location }
    }

    /// Emits a token at the `token_start` location.
    fn token(&self, kind: TokenKind) -> Token {
        Token { kind, location: self.token_start }
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

    /// Returns whether the character is a digit that's part of a number literal.
    fn is_digit_or_underscore(c: char, radix: u32) -> bool {
        c.is_digit(radix) || c == '_'
    }

    /// Collects digits into a string.
    fn collect_digits(&mut self, output: &mut String, radix: u32) -> Result<(), Error> {
        let start_location = self.location;
        let mut had_digits = false;
        while Self::is_digit_or_underscore(self.get(), radix) {
            if self.get() != '_' {
                had_digits = true;
                output.push(self.get());
            }
            self.advance();
        }
        if !had_digits {
            return Err(self.error_at(start_location, ErrorKind::UnderscoresWithoutDigits));
        }
        Ok(())
    }

    /// Parses a number.
    fn number(&mut self) -> Result<f64, Error> {
        let mut number = String::new();

        self.collect_digits(&mut number, 10)?;
        if self.get() == '.' {
            let dot = self.location.byte;
            number.push(self.get());
            self.advance();
            if Self::is_identifier_start_char(self.get()) {
                // Special case: backtrack to the dot if we find an identifier after the decimal
                // point. We want to parse this as a method call.
                self.location.byte = dot;
            } else if Self::is_digit_or_underscore(self.get(), 10) {
                self.collect_digits(&mut number, 10)?;
            } else {
                return Err(self.error(ErrorKind::MissingDigitsAfterDecimalPoint));
            }
        }
        if let 'e' | 'E' = self.get() {
            number.push(self.get());
            self.advance();
            if let '+' | '-' = self.get() {
                number.push(self.get());
                self.advance();
            }
            if !Self::is_digit_or_underscore(self.get(), 10) {
                return Err(self.error(ErrorKind::MissingExponent));
            }
            self.collect_digits(&mut number, 10)?;
        }

        // Parsing here must succeed as we only allow decimal digits and a decimal point '.'.
        let number = number.parse().unwrap();
        Ok(number)
    }

    /// Parses a 32-bit integer with the specified radix.
    fn integer(&mut self, radix: u32) -> Result<u32, Error> {
        let start_location = self.location;
        let mut number = String::new();
        self.collect_digits(&mut number, radix)?;
        if number.is_empty() {
            return Err(self.error_at(start_location, ErrorKind::UnderscoresWithoutDigits));
        }
        u32::from_str_radix(&number, radix)
            .map_err(|_| self.error_at(start_location, ErrorKind::IntLiteralOutOfRange))
    }

    /// Parses an extended `\16:123`-style integer with explicit radix.
    fn integer_with_explicit_radix(&mut self) -> Result<u32, Error> {
        let radix_location = self.location;
        let radix = self.integer(10)?;
        if !(2..=36).contains(&radix) {
            return Err(self.error_at(radix_location, ErrorKind::IntRadixOutOfRange));
        }
        if self.get() != ':' {
            return Err(self.error(ErrorKind::ColonExpectedAfterRadix));
        }
        self.advance();
        self.integer(radix)
    }

    /// Parses an extended `\b1100`-style integer with a constant radix.
    fn integer_with_constant_radix(&mut self, radix: u32) -> Result<Token, Error> {
        self.advance(); // Skip over the initial character (eg. b or x)
        let number = self.integer(radix)? as f64;
        Ok(self.token(TokenKind::Number(number)))
    }

    /// Parses a character inside of a string.
    fn string_char(&mut self) -> Result<char, Error> {
        let c = self.get();
        self.advance();
        Ok(match c {
            '\\' => {
                let escape = self.get();
                let escape_char_location = self.location;
                self.advance();
                match escape {
                    '\'' => '\'',
                    '"' => '"',
                    '\\' => '\\',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'u' => {
                        let left_brace_location = self.location;
                        if self.get() != '{' {
                            return Err(self.error(ErrorKind::UEscapeLeftBraceExpected));
                        }
                        self.advance();
                        let digits_location = self.location;
                        let mut number = String::new();
                        self.collect_digits(&mut number, 16)?;
                        if self.get() != '}' {
                            return Err(self.error_at(
                                left_brace_location,
                                ErrorKind::UEscapeMissingRightBrace,
                            ));
                        }
                        self.advance();
                        if number.is_empty() {
                            return Err(self.error_at(digits_location, ErrorKind::UEscapeEmpty));
                        }
                        // The only error here is guaranteed to be overflow.
                        let scalar_value = u32::from_str_radix(&number, 16).map_err(|_| {
                            self.error_at(digits_location, ErrorKind::UEscapeOutOfRange)
                        })?;
                        char::try_from(scalar_value).map_err(|_| {
                            self.error_at(digits_location, ErrorKind::UEscapeOutOfRange)
                        })?
                    }
                    other => {
                        return Err(
                            self.error_at(escape_char_location, ErrorKind::InvalidEscape(other))
                        )
                    }
                }
            }
            other => other,
        })
    }

    /// Parses a string.
    fn string(&mut self, raw: bool) -> Result<String, Error> {
        self.advance();
        let mut result = String::new();
        while self.get() != '"' {
            if self.get() == Self::EOF {
                return Err(self.error(ErrorKind::MissingClosingQuote));
            }
            if self.get() == '\n' {
                return Err(self.error(ErrorKind::LineBreakInStringIsNotAllowed));
            }
            result.push(if !raw {
                self.string_char()?
            } else {
                let c = self.get();
                self.advance();
                c
            });
        }
        self.advance();
        Ok(result)
    }

    /// Parses a character literal 'a'.
    fn character(&mut self) -> Result<char, Error> {
        if self.get() != '\'' {
            return Err(self.error(ErrorKind::CharacterMissingOpeningApostrophe));
        }
        self.advance();
        let c = self.string_char()?;
        if self.get() != '\'' {
            return Err(self.error(ErrorKind::CharacterMissingClosingApostrophe));
        }
        self.advance();
        Ok(c)
    }

    /// Parses a long string literal \\.
    fn long_string(&mut self) -> String {
        let mut string = String::new();
        while !matches!(self.get(), '\n' | Self::EOF) {
            // NOTE: Newlines are added in by the parser.
            // CR (from CRLF line breaks) is handled like a normal character and thus is preserved.
            string.push(self.get());
            self.advance();
        }
        string
    }

    /// Parses an extended (or "backslash") literal, that is, a literal that begins with a
    /// backslash, is discriminated by a single character following the backslash, and continues
    /// onward.
    fn extended_literal(&mut self) -> Result<Token, Error> {
        self.advance(); // Skip backslash
        match self.get() {
            'r' => {
                self.advance();
                let content = self.string(true)?;
                Ok(self.token(TokenKind::String(Rc::from(content))))
            }
            'u' => {
                self.advance();
                let c = self.character()? as u32 as f64;
                Ok(self.token(TokenKind::Number(c)))
            }
            '\\' => {
                self.advance();
                let content = self.long_string();
                Ok(self.token(TokenKind::LongString(Rc::from(content))))
            }
            '0'..='9' => {
                let number = self.integer_with_explicit_radix()? as f64;
                Ok(self.token(TokenKind::Number(number)))
            }
            'b' | 'B' => self.integer_with_constant_radix(2),
            // NOTE: Do not request an uppercase O, this is intentional so as to prevent confusion
            // between 0 and O.
            'o' => self.integer_with_constant_radix(8),
            'x' | 'X' => self.integer_with_constant_radix(16),
            other => Err(self.error(ErrorKind::InvalidBackslashLiteral(other))),
        }
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
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "func" => TokenKind::Func,
            "end" => TokenKind::End,
            "break" => TokenKind::Break,
            "return" => TokenKind::Return,

            "struct" => TokenKind::Struct,
            "impl" => TokenKind::Impl,
            "trait" => TokenKind::Trait,
            "as" => TokenKind::As,
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
                let string = self.string(false)?;
                Ok(self.token(TokenKind::String(Rc::from(string))))
            }
            '\\' => Ok(self.extended_literal()?),

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
            '>' => Ok(self.single_or_double_char_token(
                TokenKind::Greater,
                '=',
                TokenKind::GreaterEqual,
            )),

            '.' => Ok(self.single_char_token(TokenKind::Dot)),
            ':' => Ok(self.single_char_token(TokenKind::Colon)),
            '@' => Ok(self.single_char_token(TokenKind::At)),

            '(' => Ok(self.single_char_token(TokenKind::LeftParen)),
            ')' => Ok(self.single_char_token(TokenKind::RightParen)),
            '[' => Ok(self.single_char_token(TokenKind::LeftBracket)),
            ']' => Ok(self.single_char_token(TokenKind::RightBracket)),
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
