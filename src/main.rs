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
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,
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
            Token::Equal        => write!(f, "EQUAL = null"),
            Token::EqualEqual   => write!(f, "EQUAL_EQUAL == null"),
            Token::Bang         => write!(f, "BANG ! null"),
            Token::BangEqual    => write!(f, "BANG_EQUAL != null"),
            Token::Less         => write!(f, "LESS < null"),
            Token::LessEqual    => write!(f, "LESS_EQUAL <= null"),
            Token::Greater      => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::Slash        => write!(f, "SLASH / null"),
        }
    }
}

pub struct LexerError {
    line: usize,
    kind: LexerErrorKind,
}

pub enum LexerErrorKind {
    UnexpectedCharacter(char),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "[line {}] Error: {}", self.line, self.kind)
    }
}

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LexerErrorKind::UnexpectedCharacter(c) => {
                write!(f, "Unexpected character: {}", c)
            }
        }
    }
}

impl LexerError {
    pub fn unexpected_character(line: usize, ch: char) -> Self {
        LexerError { line: line, kind: LexerErrorKind::UnexpectedCharacter(ch) }
    }
}

pub struct Lexer<'a> {
    rest: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { rest: input }
    }

    fn advance(&mut self) -> Option<char> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.rest = chars.as_str();
        Some(c)
    }

    fn peek(&self) -> Option<char> {
        self.rest.chars().next()
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        loop {
            let c = self.advance()?;

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
                '=' => {
                    if self.peek() == Some('=') {
                        self.advance().unwrap();
                        Token::EqualEqual
                    } else {
                        Token::Equal
                    }
                }
                '!' => {
                    if self.peek() == Some('=') {
                        self.advance().unwrap();
                        Token::BangEqual
                    } else {
                        Token::Bang
                    }
                }
                '<' => {
                    if self.peek() == Some('=') {
                        self.advance().unwrap();
                        Token::LessEqual
                    } else {
                        Token::Less
                    }
                }
                '>' => {
                    if self.peek() == Some('=') {
                        self.advance().unwrap();
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                }
                '/' => {
                    if self.peek() == Some('/') {
                        while let Some(c) = self.advance() {
                            if c == '\n' { break }
                        }
                        continue;
                    } else {
                        Token::Slash
                    }
                }

                _ => {
                    // TODO: actually keep track of line number
                    return Some(Err(LexerError::unexpected_character(1, c)));
                },
            };

            return Some(Ok(tok));
        }
    }
}
