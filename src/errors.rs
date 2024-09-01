use crate::interpreter::Value;
use crate::lexer::Span;
use colored::Colorize;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Error<'a> {
    pub kind: ErrorKind,
    pub span: Option<Span<'a>>,
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

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(span) = self.span {
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
            ErrorKind::ReturnValue(value) => {
                write!(f, "Return value: {}", value)
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

impl<'a> Error<'a> {
    pub fn unexpected_character(span: Span<'a>) -> Self {
        let ch = span.lexeme.chars().next().expect("Should not be at EOF");
        Error {
            kind: ErrorKind::UnexpectedCharacter(ch),
            span: Some(span),
        }
    }

    pub fn unterminated_string(span: Span<'a>) -> Self {
        Error {
            kind: ErrorKind::UnterminatedString,
            span: Some(span),
        }
    }

    pub fn invalid_number(span: Span<'a>) -> Self {
        Error {
            kind: ErrorKind::InvalidNumber(span.lexeme.to_string()),
            span: Some(span),
        }
    }

    pub fn parser_error(span: Span<'a>, message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::ParserError(message.into()),
            span: Some(span),
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

    fn at_message(&self) -> Cow<str> {
        let span = match &self.kind {
            ErrorKind::ParserError(_) => self.span.expect("ParserError should have a span"),
            _ => {
                return "".into();
            }
        };

        if span.lexeme.is_empty() {
            " at end".into()
        } else {
            format!(" at '{}'", span.lexeme).into()
        }
    }
}

impl Error<'static> {
    pub fn runtime_error(message: impl Into<String>) -> Self {
        Error {
            kind: ErrorKind::RuntimeError(message.into()),
            span: None,
        }
    }

    pub fn runtime_error_on_line(message: impl Into<String>, line: usize) -> Self {
        Error {
            kind: ErrorKind::RuntimeError(message.into()),
            span: Some(Span::dummy_for_line(line)),
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
}
