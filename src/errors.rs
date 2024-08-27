use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use crate::lexer::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub line: usize,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumber(String),
    ParserError(Option<String>, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "[line {}] Error{}: {}",
            self.line,
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
            ErrorKind::ParserError(_, msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}

impl Error {
    pub fn unexpected_character(line: usize, ch: char) -> Self {
        Error {
            line,
            kind: ErrorKind::UnexpectedCharacter(ch),
        }
    }

    pub fn unterminated_string(line: usize) -> Self {
        Error {
            line,
            kind: ErrorKind::UnterminatedString,
        }
    }

    pub fn invalid_number(line: usize, s: &str) -> Self {
        Error {
            line,
            kind: ErrorKind::InvalidNumber(s.to_owned()),
        }
    }

    pub fn parser_error(line: usize, token: Option<&TokenKind<'_>>, message: &str) -> Self {
        let lexeme = token.map(|t| t.lexeme().to_owned());
        Error {
            line,
            kind: ErrorKind::ParserError(lexeme, message.to_owned()),
        }
    }

    fn at_message(&self) -> Cow<str> {
        let lexeme = match &self.kind {
            ErrorKind::ParserError(lexeme, _msg) => lexeme,
            _ => { return "".into(); },
        };

        if let Some(lexeme) = lexeme {
            format!(" at '{}'", lexeme).into()
        } else {
            " at end".into()
        }
    }
}
