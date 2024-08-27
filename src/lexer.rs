use crate::errors::*;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub struct Token<'a> {
//     kind: TokenKind<'a>,
//     span: Span,
// }

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    line: usize,
    col: usize,
    pos: usize,
    len: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'a> {
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

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {} {}", self.name(), self.lexeme(), self.value())
    }
}

impl TokenKind<'_> {
    #[rustfmt::skip]
    pub fn name(&self) -> &str {
        match self {
            TokenKind::LeftParen        => "LEFT_PAREN",
            TokenKind::RightParen       => "RIGHT_PAREN",
            TokenKind::LeftBrace        => "LEFT_BRACE",
            TokenKind::RightBrace       => "RIGHT_BRACE",
            TokenKind::Comma            => "COMMA",
            TokenKind::Dot              => "DOT",
            TokenKind::Minus            => "MINUS",
            TokenKind::Plus             => "PLUS",
            TokenKind::Semicolon        => "SEMICOLON",
            TokenKind::Star             => "STAR",
            TokenKind::Equal            => "EQUAL",
            TokenKind::EqualEqual       => "EQUAL_EQUAL",
            TokenKind::Bang             => "BANG",
            TokenKind::BangEqual        => "BANG_EQUAL",
            TokenKind::Less             => "LESS",
            TokenKind::LessEqual        => "LESS_EQUAL",
            TokenKind::Greater          => "GREATER",
            TokenKind::GreaterEqual     => "GREATER_EQUAL",
            TokenKind::Slash            => "SLASH",
            TokenKind::String(_, _)     => "STRING",
            TokenKind::Number(_, _)     => "NUMBER",
            TokenKind::Identifier(_)    => "IDENTIFIER",
            TokenKind::And              => "AND",
            TokenKind::Class            => "CLASS",
            TokenKind::Else             => "ELSE",
            TokenKind::False            => "FALSE",
            TokenKind::For              => "FOR",
            TokenKind::Fun              => "FUN",
            TokenKind::If               => "IF",
            TokenKind::Nil              => "NIL",
            TokenKind::Or               => "OR",
            TokenKind::Print            => "PRINT",
            TokenKind::Return           => "RETURN",
            TokenKind::Super            => "SUPER",
            TokenKind::This             => "THIS",
            TokenKind::True             => "TRUE",
            TokenKind::Var              => "VAR",
            TokenKind::While            => "WHILE",
        }
    }

    #[rustfmt::skip]
    pub fn lexeme(&self) -> &str {
        match self {
            TokenKind::LeftParen        => "(",
            TokenKind::RightParen       => ")",
            TokenKind::LeftBrace        => "{",
            TokenKind::RightBrace       => "}",
            TokenKind::Comma            => ",",
            TokenKind::Dot              => ".",
            TokenKind::Minus            => "-",
            TokenKind::Plus             => "+",
            TokenKind::Semicolon        => ";",
            TokenKind::Star             => "*",
            TokenKind::Equal            => "=",
            TokenKind::EqualEqual       => "==",
            TokenKind::Bang             => "!",
            TokenKind::BangEqual        => "!=",
            TokenKind::Less             => "<",
            TokenKind::LessEqual        => "<=",
            TokenKind::Greater          => ">",
            TokenKind::GreaterEqual     => ">=",
            TokenKind::Slash            => "/",
            TokenKind::Number(_, s)     => s,
            TokenKind::Identifier(s)    => s,
            TokenKind::String(_, s)     => s,
            TokenKind::And              => "and",
            TokenKind::Class            => "class",
            TokenKind::Else             => "else",
            TokenKind::False            => "false",
            TokenKind::For              => "for",
            TokenKind::Fun              => "fun",
            TokenKind::If               => "if",
            TokenKind::Nil              => "nil",
            TokenKind::Or               => "or",
            TokenKind::Print            => "print",
            TokenKind::Return           => "return",
            TokenKind::Super            => "super",
            TokenKind::This             => "this",
            TokenKind::True             => "true",
            TokenKind::Var              => "var",
            TokenKind::While            => "while",
        }
    }

    pub fn value(&self) -> Cow<str> {
        match self {
            TokenKind::Number(val, _) => {
                if *val == val.trunc() && !val.is_infinite() && !val.is_nan() {
                    format!("{}.0", val).into()
                } else {
                    format!("{}", val).into()
                }
            }
            TokenKind::String(val, _) => Cow::Borrowed(val),
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
    type Item = Result<(TokenKind<'a>, usize), Error>;

    fn next(&mut self) -> Option<Result<(TokenKind<'a>, usize), Error>> {
        loop {
            let s = self.rest;
            let c = self.advance()?;

            let tok = match c {
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '{' => TokenKind::LeftBrace,
                '}' => TokenKind::RightBrace,
                ',' => TokenKind::Comma,
                '.' => TokenKind::Dot,
                '-' => TokenKind::Minus,
                '+' => TokenKind::Plus,
                ';' => TokenKind::Semicolon,
                '*' => TokenKind::Star,
                '=' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::EqualEqual
                    } else {
                        TokenKind::Equal
                    }
                }
                '!' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::BangEqual
                    } else {
                        TokenKind::Bang
                    }
                }
                '<' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::LessEqual
                    } else {
                        TokenKind::Less
                    }
                }
                '>' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::GreaterEqual
                    } else {
                        TokenKind::Greater
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
                        TokenKind::Slash
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
                        break TokenKind::String(val, lexeme);
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
                    TokenKind::Number(val, lexeme)
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
                        "and"       => TokenKind::And,
                        "class"     => TokenKind::Class,
                        "else"      => TokenKind::Else,
                        "false"     => TokenKind::False,
                        "for"       => TokenKind::For,
                        "fun"       => TokenKind::Fun,
                        "if"        => TokenKind::If,
                        "nil"       => TokenKind::Nil,
                        "or"        => TokenKind::Or,
                        "print"     => TokenKind::Print,
                        "return"    => TokenKind::Return,
                        "super"     => TokenKind::Super,
                        "this"      => TokenKind::This,
                        "true"      => TokenKind::True,
                        "var"       => TokenKind::Var,
                        "while"     => TokenKind::While,
                        _           => TokenKind::Identifier(lexeme),
                    }
                }
                _ => {
                    return Some(Err(Error::unexpected_character(self.line, c)));
                }
            };

            return Some(Ok((tok, self.line)));
        }
    }
}
