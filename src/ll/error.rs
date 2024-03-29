//! Common things, mostly error handling-related.

use std::{borrow::Cow, fmt, rc::Rc};

/// A source location.
#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub byte: usize,
    pub line: u32,
    pub column: u32,
}

impl Location {
    /// The "uninitialized" location, used as a placeholder for proper locations in case there's a
    /// bug in codegen.
    pub const UNINIT: Self = Self {
        byte: 0,
        line: 0,
        column: 0,
    };

    /// Returns whether this location is the uninitialized location.
    pub fn is_uninit(&self) -> bool {
        self.line == 0 && self.column == 0
    }
}

/// The default location points to the first column on the first line.
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

/// A [`MethodSignature`][crate::ll::bytecode::MethodSignature] that can be rendered into text. One
/// can be obtained by calling
/// [`MethodSignature::render`][crate::ll::bytecode::MethodSignature::render].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RenderedSignature {
    pub name: Rc<str>,
    /// This arity number does not include the implicit `self` argument.
    pub parameter_count: u8,
    /// The index of the trait this signature belongs to.
    /// When `None`, the function is free and does not belong to any trait.
    pub trait_name: Option<Rc<str>>,
}

impl RenderedSignature {
    /// The empty string name is considered to be the invalid name, and will result in different
    /// `Display`.
    pub const INVALID_NAME: &'static str = "";

    pub fn invalid() -> Self {
        Self {
            name: Rc::from(Self::INVALID_NAME),
            parameter_count: 0,
            trait_name: None,
        }
    }

    pub fn is_invalid(&self) -> bool {
        &*self.name == Self::INVALID_NAME
    }
}

impl fmt::Display for RenderedSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_invalid() {
            write!(f, "(invalid method)")?;
            return Ok(());
        }

        write!(f, "{}/{}", self.name, self.parameter_count)?;

        if let Some(trait_name) = &self.trait_name {
            write!(f, " (as {trait_name})")?;
        }

        Ok(())
    }
}

/// The kind of an error.
///
/// Check the source code of the `Display` implementation to see which error kind corresponds
/// to which error message.
#[derive(Debug)]
pub enum LanguageErrorKind {
    // Lexer
    InvalidCharacter(char),
    MissingDigitsAfterDecimalPoint,
    MissingExponent,
    UnderscoresWithoutDigits,
    MissingClosingQuote,
    InvalidEscape(char),
    LineBreakInStringIsNotAllowed,
    UEscapeLeftBraceExpected,
    UEscapeMissingRightBrace,
    UEscapeEmpty,
    UEscapeOutOfRange,
    InvalidBackslashLiteral(char),
    RawStringMissingOpeningQuote,
    IntLiteralOutOfRange,
    IntRadixOutOfRange,
    ColonExpectedAfterRadix,
    CharacterMissingOpeningApostrophe,
    CharacterMissingClosingApostrophe,

    // Parser
    InvalidPrefixToken,
    InvalidInfixToken,
    MissingDo,
    MissingRightParen,
    MissingEnd,
    InvalidIfBranchToken,
    BranchAfterElse,
    IdentifierExpected,
    LeftParenExpected,
    UnexpectedEof,
    CommaExpected,
    ColonExpectedAfterDictKey,
    RightBracketExpectedToCloseEmptyDict,
    InExpectedAfterForBinding,
    RestMustBeFollowedByRightBrace,

    // Code generator
    VariableDoesNotExist(Rc<str>),
    InvalidAssignment,
    TooManyLocals,
    TooManyGlobals,
    TooManyCaptures,
    IfBranchTooLarge,
    IfExpressionTooLarge,
    OperatorRhsTooLarge,
    LoopTooLarge,
    BreakOutsideOfLoop,
    TooManyFunctions,
    TooManyArguments,
    TooManyParameters,
    TooManyMethods,
    InvalidMethodName,
    FunctionKindOutsideImpl,
    MissingFunctionBody,
    InvalidImplItem,
    MissingMethodName,
    TooManyImpls,
    MethodAlreadyImplemented(RenderedSignature),
    TooManyFields,
    FieldDoesNotExist(Rc<str>),
    FieldOutsideOfImpl,
    MissingFields(Vec<Rc<str>>),
    ListIsTooLong,
    DictIsTooLarge,
    TooManyTraits,
    InvalidTraitItem,
    TraitMethodCannotHaveBody,
    TraitAlreadyHasMethod(RenderedSignature),
    AsOutsideOfImpl,
    TooManyTraitsInImpl,
    AsCannotNest,
    FunctionKindInTrait,
    InvalidPattern,
    LetRhsMustBeAssignment,
    TupleHasTooManyElements,
    RecordHasTooManyFields,
    DuplicateRecordField(Rc<str>),
    TooManyRecords,
    RestInRecordConstructor,
    CannotAccessDiscardPattern,

    // Runtime
    TypeError {
        expected: Cow<'static, str>,
        got: Cow<'static, str>,
    },
    MethodDoesNotExist {
        type_name: Rc<str>,
        signature: RenderedSignature,
    },
    StructAlreadyImplemented,
    UserDataAlreadyBorrowed,
    DoubleMethodImplementation {
        type_name: Rc<str>,
        signature: RenderedSignature,
    },
    MethodsUnimplemented {
        type_name: Rc<str>,
        methods: Vec<RenderedSignature>,
    },

    User(Box<dyn std::error::Error>),
}

impl std::fmt::Display for LanguageErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidCharacter(c) => write!(f, "invalid character: {c:?}"),
            Self::MissingDigitsAfterDecimalPoint => write!(f, "missing digits after decimal point"),
            Self::MissingExponent => write!(f, "number exponent expected"),
            Self::UnderscoresWithoutDigits => write!(f, "at least one digit expected, got only underscores"),
            Self::MissingClosingQuote => write!(f, "missing closing quote '\"'"),
            Self::InvalidEscape(c) => write!(f, "invalid escape: \\{c}"),
            Self::LineBreakInStringIsNotAllowed => write!(f, "line breaks are not allowed in string literals; use \\n or a long string literal \\\\"),
            Self::UEscapeLeftBraceExpected => write!(f, "left brace '{{' expected after \\u escape"),
            Self::UEscapeEmpty => write!(f, "\\u escape is empty"),
            Self::UEscapeMissingRightBrace => {
                write!(f, "missing right brace '}}' for this \\u escape")
            }
            Self::UEscapeOutOfRange => write!(
                f,
                "Unicode scalar value in \\u escape is out of range (must be <= 10FFFF and outside of D800..DFFF)"
            ),
            Self::InvalidBackslashLiteral(c) => write!(f, "invalid extended literal: \\{c}"),
            Self::RawStringMissingOpeningQuote => write!(f, "missing opening quote '\"' in \\r string"),
            Self::IntLiteralOutOfRange => write!(f, "integer literal out of range"),
            Self::IntRadixOutOfRange => write!(f, "integer radix out of range; must be >= 2 and <= 36"),
            Self::ColonExpectedAfterRadix => write!(f, "colon ':' expected after integer radix"),
            Self::CharacterMissingOpeningApostrophe => write!(f, "apostrophe ' expected to begin character literal"),
            Self::CharacterMissingClosingApostrophe => write!(f, "apostrophe ' expected to end character literal"),

            Self::InvalidPrefixToken => write!(f, "invalid token in prefix position"),
            Self::InvalidInfixToken => write!(f, "invalid token in infix position"),
            Self::MissingDo => write!(f, "'do' expected"),
            Self::MissingRightParen => write!(f, "missing right parenthesis ')'"),
            Self::MissingEnd => write!(f, "missing 'end'"),
            Self::InvalidIfBranchToken => write!(f, "'elif', 'else', or 'end' expected"),
            Self::BranchAfterElse => write!(f, "extraneous branch after 'else'"),
            Self::IdentifierExpected => write!(f, "identifier expected"),
            Self::LeftParenExpected => write!(f, "left parenthesis '(' expected"),
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
            Self::CommaExpected => write!(f, "comma ',' expected"),
            Self::ColonExpectedAfterDictKey => write!(f, "colon ':' expected after dict key"),
            Self::RightBracketExpectedToCloseEmptyDict => write!(f, "right bracket ']' expected to close empty dict literal [:]"),
            Self::MissingFunctionBody => write!(f, "missing function body ('= expression')"),
            Self::InExpectedAfterForBinding => write!(f, "'in' expected after 'for' loop variable binding"),
            Self::RestMustBeFollowedByRightBrace => write!(f, "'..' in record pattern cannot be followed by any elements"),
            Self::RestInRecordConstructor => write!(f, "'..' may only appear in record patterns"),

            Self::VariableDoesNotExist(name) => write!(f, "variable '{name}' does not exist"),
            Self::InvalidAssignment => write!(f, "invalid left hand side of assignment"),
            Self::TooManyLocals => write!(f, "too many local variables"),
            Self::TooManyGlobals => write!(f, "too many global variables"),
            Self::TooManyCaptures => write!(f, "too many variables captured in the closure"),
            Self::IfBranchTooLarge => write!(f, "'if' branch is too large"),
            Self::IfExpressionTooLarge => write!(f, "'if' expression is too large"),
            Self::OperatorRhsTooLarge => write!(f, "the right-hand side of the operator is too large"),
            Self::LoopTooLarge => write!(f, "loop is too large"),
            Self::BreakOutsideOfLoop => write!(f, "'break' cannot be used outside of a loop"),
            Self::TooManyFunctions => write!(f, "too many unique functions"),
            Self::TooManyArguments => write!(f, "too many arguments"),
            Self::TooManyParameters => write!(f, "too many parameters"),
            Self::TooManyMethods => write!(f, "too many instance functions with different signatures"),
            Self::InvalidMethodName => write!(f, "method name must be an identifier"),
            Self::FunctionKindOutsideImpl => write!(
                f,
                "function kinds (static, constructor) can only be used in 'impl' blocks"
            ),
            Self::InvalidImplItem => write!(f, "only functions and 'as' blocks are allowed in 'impl' blocks"),
            Self::MissingMethodName => write!(f, "missing method name"),
            Self::TooManyImpls => write!(f, "too many 'impl' blocks"),
            Self::MethodAlreadyImplemented(signature) => {
                write!(f, "method {signature} is already implemented")
            }
            Self::TooManyFields => write!(f, "too many fields"),
            Self::FieldOutsideOfImpl => {
                write!(f, "fields cannot be referenced outside of 'impl' blocks")
            }
            Self::FieldDoesNotExist(name) => write!(f, "field '@{name}' does not exist"),
            Self::MissingFields(fields) => {
                let fields: Vec<_> = fields.iter().map(|name| format!("@{name}")).collect();
                let fields = fields.join(", ");
                write!(
                    f,
                    "the following fields were not assigned in this constructor: {fields}"
                )
            }
            Self::ListIsTooLong => write!(f, "list literal has too many elements"),
            Self::DictIsTooLarge => write!(f, "dict literal has too many pairs"),
            Self::TooManyTraits => write!(f, "too many traits"),
            Self::InvalidTraitItem => write!(f, "only function prototypes are allowed in traits"),
            Self::TraitMethodCannotHaveBody => write!(f, "trait functions cannot have bodies"),
            Self::TraitAlreadyHasMethod(signature) => {
                write!(f, "trait already declares the method {signature}")
            }
            Self::AsOutsideOfImpl => write!(f, "'as' is not allowed outside of 'impl' blocks"),
            Self::TooManyTraitsInImpl => write!(f, "too many 'as' blocks in 'impl'"),
            Self::AsCannotNest => write!(f, "'as' blocks cannot nest"),
            Self::FunctionKindInTrait => write!(f, "trait functions must be instance methods (cannot be constructors nor statics)"),
            Self::InvalidPattern => write!(f, "invalid pattern for destructuring into variables"),
            Self::LetRhsMustBeAssignment => write!(f, "the right hand side of 'let' must be an assignment, like 'let x = y'"),
            Self::TupleHasTooManyElements => write!(f, "tuple has too many elements"),
            Self::RecordHasTooManyFields => write!(f, "record has too many fields"),
            Self::DuplicateRecordField(name) => write!(f, "duplicate record field '{name}'"),
            Self::TooManyRecords => write!(f, "too many distinct records"),

            Self::TypeError { expected, got } => {
                write!(f, "type mismatch, expected {expected} but got {got}")
            }
            Self::MethodDoesNotExist { type_name, signature } => write!(f, "method {signature} is not defined for {type_name}"),
            Self::StructAlreadyImplemented => write!(f, "this struct is already implemented"),
            Self::UserDataAlreadyBorrowed => write!(f, "this user data is already borrowed"),
            Self::DoubleMethodImplementation { type_name, signature } => {
                write!(f, "method {signature} is already implemented by {type_name}")
            }
            Self::MethodsUnimplemented { type_name, methods } => {
                writeln!(f, "{type_name} is missing the following trait methods:")?;
                for (i, signature) in methods.iter().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "    - {signature}")?;
                }
                Ok(())
            }
            Self::CannotAccessDiscardPattern => {
                write!(f, "'_' is a used for discarding values in variable declarations and cannot be used in expressions")
            }

            Self::User(error) => write!(f, "{error}"),
        }
    }
}

/// An entry of a stack trace.
#[derive(Debug)]
pub struct StackTraceEntry {
    /// The name of the current function.
    pub function_name: Rc<str>,
    /// The name of the module the function was in.
    pub module_name: Rc<str>,
    /// The source location in the module.
    pub location: Location,
}

/// An error.
#[derive(Debug)]
pub enum LanguageError {
    /// A compile-time error.
    Compile {
        kind: LanguageErrorKind,
        module_name: Rc<str>,
        location: Location,
    },
    /// A runtime error.
    Runtime {
        kind: LanguageErrorKind,
        call_stack: Vec<StackTraceEntry>,
    },
}

impl std::fmt::Display for LanguageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct FileLocation<'a>(&'a str, Location);

        impl std::fmt::Display for FileLocation<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let Self(file, location) = self;
                if location.is_uninit() {
                    write!(f, "{file}")
                } else {
                    write!(f, "{file}:{location}")
                }
            }
        }

        match self {
            LanguageError::Compile {
                kind,
                module_name,
                location,
            } => {
                write!(f, "{module_name}:{location}: error: {kind}")
            }
            LanguageError::Runtime { kind, call_stack } => {
                writeln!(f, "error: {kind}")?;
                write!(f, "stack traceback (most recent call first):")?;
                let file_location_width = call_stack
                    .iter()
                    .map(|entry| {
                        FileLocation(&entry.module_name, entry.location)
                            .to_string()
                            .len()
                    })
                    .max()
                    .unwrap_or(20);
                for entry in call_stack.iter().rev() {
                    write!(
                        f,
                        "\n    {:width$}  {}",
                        // Well this is a bit horrible.
                        FileLocation(&entry.module_name, entry.location).to_string(),
                        entry.function_name,
                        width = file_location_width,
                    )?;
                }
                Ok(())
            }
        }
    }
}
