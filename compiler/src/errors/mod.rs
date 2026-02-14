#![allow(unused_assignments)]

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Represents a location in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        SourceSpan::new(span.start.into(), (span.end - span.start).into())
    }
}

/// Lexer errors
#[derive(Error, Debug, Diagnostic)]
pub enum LexerError {
    #[error("Unexpected character '{ch}'")]
    #[diagnostic(code(lexer::unexpected_char))]
    UnexpectedCharacter {
        ch: char,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Unterminated string literal")]
    #[diagnostic(code(lexer::unterminated_string))]
    UnterminatedString {
        #[label("string starts here")]
        span: SourceSpan,
    },

    #[error("Unterminated block comment")]
    #[diagnostic(code(lexer::unterminated_comment))]
    UnterminatedComment {
        #[label("comment starts here")]
        span: SourceSpan,
    },

    #[error("Invalid number literal")]
    #[diagnostic(code(lexer::invalid_number))]
    InvalidNumber {
        #[label("here")]
        span: SourceSpan,
    },
}

/// Parser errors
#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Expected {expected}, found {found}")]
    #[diagnostic(code(parser::unexpected_token))]
    UnexpectedToken {
        expected: String,
        found: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Expected expression")]
    #[diagnostic(code(parser::expected_expression))]
    ExpectedExpression {
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Expected 'end' to close function definition")]
    #[diagnostic(code(parser::expected_end))]
    ExpectedEnd {
        #[label("function started here")]
        start_span: SourceSpan,
        #[label("expected 'end' here")]
        span: SourceSpan,
    },
}

/// Type checking errors
#[derive(Error, Debug, Diagnostic)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, found {found}")]
    #[diagnostic(code(types::mismatch))]
    TypeMismatch {
        expected: String,
        found: String,
        #[label("expected {expected} here")]
        span: SourceSpan,
    },

    #[error("Undefined variable '{name}'")]
    #[diagnostic(code(types::undefined_variable))]
    UndefinedVariable {
        name: String,
        #[label("not found in scope")]
        span: SourceSpan,
    },

    #[error("Undefined function '{name}'")]
    #[diagnostic(code(types::undefined_function))]
    UndefinedFunction {
        name: String,
        #[label("not found")]
        span: SourceSpan,
    },

    #[error("Cannot apply operator '{op}' to types {lhs} and {rhs}")]
    #[diagnostic(code(types::invalid_operator))]
    InvalidOperator {
        op: String,
        lhs: String,
        rhs: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Function '{name}' expects {expected} arguments, found {found}")]
    #[diagnostic(code(types::wrong_arity))]
    WrongArity {
        name: String,
        expected: usize,
        found: usize,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Not all code paths in function '{name}' return a value")]
    #[diagnostic(code(types::missing_return))]
    MissingReturn {
        name: String,
        #[label("function may exit without returning")]
        span: SourceSpan,
    },

    #[error("Unreachable code")]
    #[diagnostic(code(types::unreachable_code))]
    UnreachableCode {
        #[label("this statement is unreachable")]
        span: SourceSpan,
    },

    #[error("Undefined struct '{name}'")]
    #[diagnostic(code(types::undefined_struct))]
    UndefinedStruct {
        name: String,
        #[label("not found")]
        span: SourceSpan,
    },

    #[error("Undefined field '{field}' on struct '{struct_name}'")]
    #[diagnostic(code(types::undefined_field))]
    UndefinedField {
        struct_name: String,
        field: String,
        #[label("no such field")]
        span: SourceSpan,
    },
}

/// Runtime errors (interpreter backend)
#[derive(Error, Debug, Diagnostic)]
pub enum RuntimeError {
    #[error("No `main` function found")]
    #[diagnostic(code(runtime::missing_main))]
    MissingMain,

    #[error("Undefined function '{name}'")]
    #[diagnostic(code(runtime::undefined_function))]
    UndefinedFunction {
        name: String,
        #[label("called here")]
        span: SourceSpan,
    },

    #[error("Function '{name}' expects {expected} arguments, found {found}")]
    #[diagnostic(code(runtime::wrong_arity))]
    WrongArity {
        name: String,
        expected: usize,
        found: usize,
        #[label("called here")]
        span: SourceSpan,
    },

    #[error("Undefined variable '{name}'")]
    #[diagnostic(code(runtime::undefined_variable))]
    UndefinedVariable {
        name: String,
        #[label("used here")]
        span: SourceSpan,
    },

    #[error("Type error: {message}")]
    #[diagnostic(code(runtime::type_error))]
    TypeError {
        message: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Division by zero")]
    #[diagnostic(code(runtime::division_by_zero))]
    DivisionByZero {
        #[label("here")]
        span: SourceSpan,
    },
}

/// Codegen errors
#[derive(Error, Debug, Diagnostic)]
pub enum CodegenError {
    #[error("LLVM error: {message}")]
    #[diagnostic(code(codegen::llvm))]
    LlvmError { message: String },
}
