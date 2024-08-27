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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
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

    String(&'a str),
    Number(f64, &'a str),
    Identifier(&'a str),

    // keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Token::LeftParen        => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen       => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace        => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace       => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma            => write!(f, "COMMA , null"),
            Token::Dot              => write!(f, "DOT . null"),
            Token::Minus            => write!(f, "MINUS - null"),
            Token::Plus             => write!(f, "PLUS + null"),
            Token::Semicolon        => write!(f, "SEMICOLON ; null"),
            Token::Star             => write!(f, "STAR * null"),
            Token::Equal            => write!(f, "EQUAL = null"),
            Token::EqualEqual       => write!(f, "EQUAL_EQUAL == null"),
            Token::Bang             => write!(f, "BANG ! null"),
            Token::BangEqual        => write!(f, "BANG_EQUAL != null"),
            Token::Less             => write!(f, "LESS < null"),
            Token::LessEqual        => write!(f, "LESS_EQUAL <= null"),
            Token::Greater          => write!(f, "GREATER > null"),
            Token::GreaterEqual     => write!(f, "GREATER_EQUAL >= null"),
            Token::Slash            => write!(f, "SLASH / null"),
            Token::String(s)        => write!(f, "STRING \"{}\" {}", s, s),
            Token::Number(n, s)     => {
                if *n == n.trunc() && !n.is_infinite() && !n.is_nan() {
                    write!(f, "NUMBER {} {}.0", s, n)
                } else {
                    write!(f, "NUMBER {} {}", s, n)
                }
            }
            Token::Identifier(s)    => write!(f, "IDENTIFIER {} null", s),
            Token::And              => write!(f, "AND and null"),
            Token::Class            => write!(f, "CLASS class null"),
            Token::Else             => write!(f, "ELSE else null"),
            Token::False            => write!(f, "FALSE false null"),
            Token::For              => write!(f, "FOR for null"),
            Token::Fun              => write!(f, "FUN fun null"),
            Token::If               => write!(f, "IF if null"),
            Token::Nil              => write!(f, "NIL nil null"),
            Token::Or               => write!(f, "OR or null"),
            Token::Print            => write!(f, "PRINT print null"),
            Token::Return           => write!(f, "RETURN return null"),
            Token::Super            => write!(f, "SUPER super null"),
            Token::This             => write!(f, "THIS this null"),
            Token::True             => write!(f, "TRUE true null"),
            Token::Var              => write!(f, "VAR var null"),
            Token::While            => write!(f, "WHILE while null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LexerError {
    line: usize,
    kind: LexerErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    InvalidNumber(String),
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
            LexerErrorKind::UnterminatedString => {
                write!(f, "Unterminated string.")
            }
            LexerErrorKind::InvalidNumber(s) => {
                write!(f, "Invalid number: {}", s)
            }
        }
    }
}

impl LexerError {
    pub fn unexpected_character(line: usize, ch: char) -> Self {
        LexerError { line: line, kind: LexerErrorKind::UnexpectedCharacter(ch) }
    }

    pub fn unterminated_string(line: usize) -> Self {
        LexerError { line: line, kind: LexerErrorKind::UnterminatedString }
    }

    pub fn invalid_number(line: usize, s: &str) -> Self {
        LexerError { line: line, kind: LexerErrorKind::InvalidNumber(s.to_owned()) }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    rest: &'a str,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { rest: input, line: 1 }
    }

    fn advance(&mut self) -> Option<char> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.rest = chars.as_str();
        if c == '\n' {
            self.line += 1;
        }
        Some(c)
    }

    fn peek(&self) -> Option<char> {
        self.rest.chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.rest.chars();
        chars.next()?;
        chars.next()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexerError>;

    fn next(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        loop {
            let s = self.rest;
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
                ' ' | '\t' | '\r' | '\n' => {
                    continue;
                }
                '"' => {
                    let line = self.line;
                    let mut len = 1;
                    loop {
                        let Some(c) = self.advance() else {
                            return Some(Err(LexerError::unterminated_string(line)));
                        };
                        len += c.len_utf8();
                        if c != '"' { continue }
                        break Token::String(&s[1..len-1]);
                    }
                }
                '0'..='9' => {
                    let mut len = 1;
                    while let Some(c) = self.peek() {
                        if !c.is_digit(10) { break }
                        len += c.len_utf8();
                        self.advance();
                    }

                    if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_digit(10)) {
                        self.advance();  // dot
                        len += 1;

                        while let Some(c) = self.peek() {
                            if !c.is_digit(10) { break }
                            len += c.len_utf8();
                            self.advance();
                        }
                    }

                    let s = &s[..len];
                    let n: f64 = match s.parse() {
                        Ok(n) => n,
                        Err(_) => {
                            return Some(Err(LexerError::invalid_number(self.line, s)));
                        }
                    };
                    Token::Number(n, s)
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut len = 1;
                    while let Some(c) = self.peek() {
                        if !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_') { break }
                        len += c.len_utf8();
                        self.advance();
                    }
                    let s = &s[..len];

                    match s {
                        "and"       => Token::And,
                        "class"     => Token::Class,
                        "else"      => Token::Else,
                        "false"     => Token::False,
                        "for"       => Token::For,
                        "fun"       => Token::Fun,
                        "if"        => Token::If,
                        "nil"       => Token::Nil,
                        "or"        => Token::Or,
                        "print"     => Token::Print,
                        "return"    => Token::Return,
                        "super"     => Token::Super,
                        "this"      => Token::This,
                        "true"      => Token::True,
                        "var"       => Token::Var,
                        "while"     => Token::While,
                        _           => Token::Identifier(s),
                    }
                }
                _ => {
                    return Some(Err(LexerError::unexpected_character(self.line, c)));
                },
            };

            return Some(Ok(tok));
        }
    }
}
