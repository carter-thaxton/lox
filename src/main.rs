use std::fmt::{Display, Formatter};
use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut lexer_error = false;
            let lexer = Lexer::new(&file_contents);
            for result in lexer {
                match result {
                    Ok(token) => {
                        println!("{}", token);
                    },
                    Err(err) => {
                        eprintln!("{}", err);
                        lexer_error = true;
                    },
                }
            }
            println!("EOF  null");

            if lexer_error {
                std::process::exit(65);
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}

pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Token::LeftParen    => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen   => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace    => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace   => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma        => write!(f, "COMMA , null"),
            Token::Dot          => write!(f, "DOT . null"),
            Token::Minus        => write!(f, "MINUS - null"),
            Token::Plus         => write!(f, "PLUS + null"),
            Token::Semicolon    => write!(f, "SEMICOLON ; null"),
            Token::Star         => write!(f, "STAR * null"),
        }
    }
}

pub struct LexerError {
    line: usize,
    kind: LexerErrorKind,
}

pub enum LexerErrorKind {
    UnrecognizedCharacter(char),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "[line {}] Error: {}", self.line, self.kind)
    }
}

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LexerErrorKind::UnrecognizedCharacter(c) => {
                write!(f, "Unexpected character: {}", c)
            }
        }
    }
}

impl LexerError {
    pub fn unrecognized_character(line: usize, ch: char) -> Self {
        LexerError { line: line, kind: LexerErrorKind::UnrecognizedCharacter(ch) }
    }
}

pub struct Lexer<'a> {
    rest: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { rest: input }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.rest = chars.as_str();

        let tok = match c {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '-' => Token::Minus,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '*' => Token::Star,
            _ => {
                // TODO: actually keep track of line number
                return Some(Err(LexerError::unrecognized_character(1, c)));
            },
        };

        Some(Ok(tok))
    }
}
