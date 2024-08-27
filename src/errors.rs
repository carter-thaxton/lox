use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Error {
    line: usize,
    kind: ErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumber(String),
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
        }
    }
}

impl Error {
    pub fn unexpected_character(line: usize, ch: char) -> Self {
        Error {
            line: line,
            kind: ErrorKind::UnexpectedCharacter(ch),
        }
    }

    pub fn unterminated_string(line: usize) -> Self {
        Error {
            line: line,
            kind: ErrorKind::UnterminatedString,
        }
    }

    pub fn invalid_number(line: usize, s: &str) -> Self {
        Error {
            line: line,
            kind: ErrorKind::InvalidNumber(s.to_owned()),
        }
    }

    fn at_message(&self) -> &str {
        // TODO: implement parser errors showing current lexeme
        ""
    }
}
