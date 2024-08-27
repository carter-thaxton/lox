use crate::lexer::Span;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Error<'a> {
    pub kind: ErrorKind,
    pub span: Span<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumber(String),
    ParserError(String),
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "[line {}] Error{}: {}",
            self.span.line,
            self.at_message(),
            self.kind
        )
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ErrorKind::UnexpectedCharacter(c) => {
                write!(f, "Unexpected character: {}", c)
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
        }
    }
}

impl<'a> Error<'a> {
    pub fn unexpected_character(span: Span<'a>) -> Self {
        let ch = span.lexeme.chars().next().expect("Should not be at EOF");
        Error {
            kind: ErrorKind::UnexpectedCharacter(ch),
            span,
        }
    }

    pub fn unterminated_string(span: Span<'a>) -> Self {
        Error {
            kind: ErrorKind::UnterminatedString,
            span,
        }
    }

    pub fn invalid_number(span: Span<'a>) -> Self {
        Error {
            kind: ErrorKind::InvalidNumber(span.lexeme.to_string()),
            span,
        }
    }

    pub fn parser_error(span: Span<'a>, message: &str) -> Self {
        Error {
            kind: ErrorKind::ParserError(message.to_owned()),
            span: span,
        }
    }

    fn at_message(&self) -> Cow<str> {
        let span = match &self.kind {
            ErrorKind::ParserError(_) => self.span,
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
