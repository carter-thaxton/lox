use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use crate::errors::*;

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

    String(&'a str, &'a str),
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
        write!(f, "{} {} {}", self.name(), self.lexeme(), self.value())
    }
}

impl Token<'_> {
    #[rustfmt::skip]
    pub fn name(&self) -> &str {
        match self {
            Token::LeftParen        => "LEFT_PAREN",
            Token::RightParen       => "RIGHT_PAREN",
            Token::LeftBrace        => "LEFT_BRACE",
            Token::RightBrace       => "RIGHT_BRACE",
            Token::Comma            => "COMMA",
            Token::Dot              => "DOT",
            Token::Minus            => "MINUS",
            Token::Plus             => "PLUS",
            Token::Semicolon        => "SEMICOLON",
            Token::Star             => "STAR",
            Token::Equal            => "EQUAL",
            Token::EqualEqual       => "EQUAL_EQUAL",
            Token::Bang             => "BANG",
            Token::BangEqual        => "BANG_EQUAL",
            Token::Less             => "LESS",
            Token::LessEqual        => "LESS_EQUAL",
            Token::Greater          => "GREATER",
            Token::GreaterEqual     => "GREATER_EQUAL",
            Token::Slash            => "SLASH",
            Token::String(_, _)     => "STRING",
            Token::Number(_, _)     => "NUMBER",
            Token::Identifier(_)    => "IDENTIFIER",
            Token::And              => "AND",
            Token::Class            => "CLASS",
            Token::Else             => "ELSE",
            Token::False            => "FALSE",
            Token::For              => "FOR",
            Token::Fun              => "FUN",
            Token::If               => "IF",
            Token::Nil              => "NIL",
            Token::Or               => "OR",
            Token::Print            => "PRINT",
            Token::Return           => "RETURN",
            Token::Super            => "SUPER",
            Token::This             => "THIS",
            Token::True             => "TRUE",
            Token::Var              => "VAR",
            Token::While            => "WHILE",
        }
    }

    #[rustfmt::skip]
    pub fn lexeme(&self) -> &str {
        match self {
            Token::LeftParen        => "(",
            Token::RightParen       => ")",
            Token::LeftBrace        => "{",
            Token::RightBrace       => "}",
            Token::Comma            => ",",
            Token::Dot              => ".",
            Token::Minus            => "-",
            Token::Plus             => "+",
            Token::Semicolon        => ";",
            Token::Star             => "*",
            Token::Equal            => "=",
            Token::EqualEqual       => "==",
            Token::Bang             => "!",
            Token::BangEqual        => "!=",
            Token::Less             => "<",
            Token::LessEqual        => "<=",
            Token::Greater          => ">",
            Token::GreaterEqual     => ">=",
            Token::Slash            => "/",
            Token::Number(_, s)     => s,
            Token::Identifier(s)    => s,
            Token::String(_, s)     => s,
            Token::And              => "and",
            Token::Class            => "class",
            Token::Else             => "else",
            Token::False            => "false",
            Token::For              => "for",
            Token::Fun              => "fun",
            Token::If               => "if",
            Token::Nil              => "nil",
            Token::Or               => "or",
            Token::Print            => "print",
            Token::Return           => "return",
            Token::Super            => "super",
            Token::This             => "this",
            Token::True             => "true",
            Token::Var              => "var",
            Token::While            => "while",
        }
    }

    pub fn value(&self) -> Cow<str> {
        match self {
            Token::Number(val, _) => {
                if *val == val.trunc() && !val.is_infinite() && !val.is_nan() {
                    format!("{}.0", val).into()
                } else {
                    format!("{}", val).into()
                }
            }
            Token::String(val, _) => Cow::Borrowed(val),
            _ => "null".into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    rest: &'a str,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            rest: input,
            line: 1,
        }
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
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Result<Token<'a>, Error>> {
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
                        self.advance();
                        Token::EqualEqual
                    } else {
                        Token::Equal
                    }
                }
                '!' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::BangEqual
                    } else {
                        Token::Bang
                    }
                }
                '<' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::LessEqual
                    } else {
                        Token::Less
                    }
                }
                '>' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                }
                '/' => {
                    if self.peek() == Some('/') {
                        while let Some(c) = self.advance() {
                            if c == '\n' {
                                break;
                            }
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
                            return Some(Err(Error::unterminated_string(line)));
                        };
                        len += c.len_utf8();
                        if c != '"' {
                            continue;
                        }
                        let lexeme = &s[..len];
                        let val = &s[1..len - 1];
                        break Token::String(val, lexeme);
                    }
                }
                '0'..='9' => {
                    let mut len = 1;
                    while let Some(c) = self.peek() {
                        if !c.is_digit(10) {
                            break;
                        }
                        len += c.len_utf8();
                        self.advance();
                    }

                    if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_digit(10))
                    {
                        self.advance(); // dot
                        len += 1;

                        while let Some(c) = self.peek() {
                            if !c.is_digit(10) {
                                break;
                            }
                            len += c.len_utf8();
                            self.advance();
                        }
                    }

                    let lexeme = &s[..len];
                    let val: f64 = match lexeme.parse() {
                        Ok(val) => val,
                        Err(_) => {
                            return Some(Err(Error::invalid_number(self.line, lexeme)));
                        }
                    };
                    Token::Number(val, lexeme)
                }
                #[rustfmt::skip]
                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut len = 1;
                    while let Some(c) = self.peek() {
                        if !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_') { break }
                        len += c.len_utf8();
                        self.advance();
                    }
                    let lexeme = &s[..len];

                    match lexeme {
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
                        _           => Token::Identifier(lexeme),
                    }
                }
                _ => {
                    return Some(Err(Error::unexpected_character(self.line, c)));
                }
            };

            return Some(Ok(tok));
        }
    }
}
