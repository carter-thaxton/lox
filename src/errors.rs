use crate::lexer::Span;
use crate::runtime::Value;
use colored::Colorize;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Option<ErrorSpan>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorSpan {
    line: usize,
    col: usize,
    lexeme: Option<String>,
}

impl ErrorSpan {
    fn dummy_for_line(line: usize) -> Self {
        ErrorSpan {
            line,
            col: 0,
            lexeme: None,
        }
    }

    fn dummy_for_line_at_token(line: usize, token: impl Into<String>) -> Self {
        ErrorSpan {
            line,
            col: 0,
            lexeme: Some(token.into()),
        }
    }
}

impl From<Span<'_>> for ErrorSpan {
    fn from(span: Span<'_>) -> Self {
        ErrorSpan {
            line: span.line,
            col: span.col,
            lexeme: Some(span.lexeme.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumber(String),
    ParserError(String),
    RuntimeError(String),

    ReturnValue(Value),
    BreakLoop,
    ContinueLoop,

    TestExpectedParserError(String),
    TestExpectedRuntimeError(String),
    TestOutputMismatch(String, String),
    TestOutputMissing(String),
    TestOutputUnexpected(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(span) = &self.span {
            write!(
                f,
                "[line {}] Error{}: {}",
                span.line,
                self.at_message(),
                self.kind
            )
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ErrorKind::UnexpectedCharacter(_) => {
                write!(f, "Unexpected character.") // ideally, would show invalid character, but this matches tests.
            }
            ErrorKind::UnterminatedString => {
                write!(f, "Unterminated string.")
            }
            ErrorKind::InvalidNumber(s) => {
                write!(f, "Invalid number: {}", s)
            }
            ErrorKind::ParserError(msg) => {
                write!(f, "{}", msg)
            }
            ErrorKind::RuntimeError(msg) => {
                write!(f, "{}", msg)
            }

            // == TEST ==
            ErrorKind::TestExpectedParserError(msg) => {
                write!(f, "{}: Expected parser error: {}", "FAIL".red(), msg)
            }
            ErrorKind::TestExpectedRuntimeError(msg) => {
                write!(f, "{}: Expected runtime error: {}", "FAIL".red(), msg)
            }
            ErrorKind::TestOutputMismatch(expected, actual) => {
                write!(
                    f,
                    "{}: Expected output: {} - got: {}",
                    "FAIL".red(),
                    expected,
                    actual
                )
            }
            ErrorKind::TestOutputMissing(expected) => {
                write!(
                    f,
                    "{}: Expected output: {} - got nothing",
                    "FAIL".red(),
                    expected
                )
            }
            ErrorKind::TestOutputUnexpected(actual) => {
                write!(f, "{}: Unexpected output: {}", "FAIL".red(), actual)
            }

            // runtime control-flow
            ErrorKind::ReturnValue(value) => {
                write!(f, "return: {}", value)
            }
            ErrorKind::BreakLoop => {
                write!(f, "break")
            }
            ErrorKind::ContinueLoop => {
                write!(f, "continue")
            }
        }
    }
}

impl Error {
    pub fn unexpected_character(span: Span<'_>) -> Self {
        let ch = span.lexeme.chars().next().expect("Should not be at EOF");
        Error {
            kind: ErrorKind::UnexpectedCharacter(ch),
            span: Some(span.into()),
        }
    }

    pub fn unterminated_string(span: Span<'_>) -> Self {
        Error {
            kind: ErrorKind::UnterminatedString,
            span: Some(span.into()),
        }
    }

    pub fn invalid_number(span: Span<'_>) -> Self {
        Error {
            kind: ErrorKind::InvalidNumber(span.lexeme.to_string()),
            span: Some(span.into()),
        }
    }

    pub fn parser_error(span: impl Into<ErrorSpan>, message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::ParserError(message.into()),
            span: Some(span.into()),
        }
    }

    pub fn parser_error_on_line(line: usize, message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::ParserError(message.into()),
            span: Some(ErrorSpan::dummy_for_line(line)),
        }
    }

    pub fn parser_error_on_line_at_token(line: usize, token: impl Into<String>, message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::ParserError(message.into()),
            span: Some(ErrorSpan::dummy_for_line_at_token(line, token)),
        }
    }

    pub fn test_expected_parser_error(message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::TestExpectedParserError(message.into()),
            span: None,
        }
    }

    pub fn test_expected_runtime_error(message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::TestExpectedRuntimeError(message.into()),
            span: None,
        }
    }

    pub fn test_output_mismatch(expected: impl Into<String>, actual: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::TestOutputMismatch(expected.into(), actual.into()),
            span: None,
        }
    }

    pub fn test_output_missing(expected: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::TestOutputMissing(expected.into()),
            span: None,
        }
    }

    pub fn test_output_unexpected(actual: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::TestOutputUnexpected(actual.into()),
            span: None,
        }
    }

    pub fn is_test(&self) -> bool {
        matches!(
            self.kind,
            ErrorKind::TestExpectedParserError(_)
                | ErrorKind::TestExpectedRuntimeError(_)
                | ErrorKind::TestOutputMismatch(_, _)
                | ErrorKind::TestOutputMissing(_)
        )
    }

    pub fn runtime_error(message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::RuntimeError(message.into()),
            span: None,
        }
    }

    pub fn runtime_error_on_line(message: impl Into<String>, line: usize) -> Self {
        Error {
            kind: ErrorKind::RuntimeError(message.into()),
            span: Some(ErrorSpan::dummy_for_line(line)),
        }
    }

    pub fn return_value(value: Value) -> Self {
        Error {
            kind: ErrorKind::ReturnValue(value),
            span: None,
        }
    }

    pub fn break_loop() -> Self {
        Error {
            kind: ErrorKind::BreakLoop,
            span: None,
        }
    }

    pub fn continue_loop() -> Self {
        Error {
            kind: ErrorKind::ContinueLoop,
            span: None,
        }
    }

    fn at_message(&self) -> Cow<str> {
        let span: &ErrorSpan = match &self.kind {
            ErrorKind::ParserError(_) => &self.span.as_ref().expect("ParserError should have a span"),
            _ => {
                return "".into();
            }
        };

        if let Some(lexeme) = &span.lexeme {
            format!(" at '{}'", lexeme).into()
        } else {
            " at end".into()
        }
    }
}
