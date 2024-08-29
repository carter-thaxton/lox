use crate::errors::*;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span<'a> {
    pub line: usize,
    pub col: usize,
    pub lexeme: &'a str,
}

impl Span<'_> {
    pub fn dummy_for_line(line: usize) -> Self {
        Span {
            line: line,
            col: 0,
            lexeme: "",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

    // tokens with data
    String(Cow<'a, str>),
    Number(f64),
    Identifier(&'a str),

    // specially formatted comments, used for testing
    ExpectOutput(&'a str),
    ExpectParserError(&'a str),
    ExpectRuntimeError(&'a str),

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
        write!(
            f,
            "{} {} {}",
            self.kind.name(),
            self.span.lexeme,
            self.kind.value()
        )
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
            TokenKind::String(_)        => "STRING",
            TokenKind::Number(_)        => "NUMBER",
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

            TokenKind::ExpectOutput(_)          => "EXPECT",
            TokenKind::ExpectParserError(_)     => "PARSER_ERROR",
            TokenKind::ExpectRuntimeError(_)    => "RUNTIME_ERROR",
        }
    }

    pub fn value(&self) -> Cow<str> {
        match self {
            TokenKind::Number(val) => {
                if *val == val.trunc() && !val.is_infinite() && !val.is_nan() {
                    format!("{}.0", val).into()
                } else {
                    format!("{}", val).into()
                }
            }
            TokenKind::String(val) => Cow::Borrowed(val),
            _ => "null".into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    rest: &'a str,
    line: usize,
    col: usize,

    lexeme_start: &'a str,
    lexeme_line: usize,
    lexeme_col: usize,
    lexeme_len: usize,

    enable_test_comments: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, enable_test_comments: bool) -> Self {
        Lexer {
            rest: input,
            line: 1,
            col: 0,

            lexeme_start: input,
            lexeme_line: 0,
            lexeme_col: 0,
            lexeme_len: 0,

            enable_test_comments,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        let len = c.len_utf8();
        self.rest = chars.as_str();
        self.col += len;
        self.lexeme_len += len;
        if c == '\n' {
            self.line += 1;
            self.col = 0;
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

    // starts keeping track of a lexeme
    // returns span for next char, appropriate for errors during lexing
    fn start_lexeme(&mut self) -> Span<'a> {
        let next_char_len = match self.peek() {
            Some(c) => c.len_utf8(),
            None => 0,
        };

        self.lexeme_start = self.rest;
        self.lexeme_line = self.line;
        self.lexeme_col = self.col;
        self.lexeme_len = 0;

        Span {
            line: self.line,
            col: self.col,
            lexeme: &self.rest[..next_char_len],
        }
    }

    // end a lexeme, resetting the lexeme state
    // returns span for string since calling start_lexeme
    fn end_lexeme(&mut self) -> Span<'a> {
        if self.lexeme_line == 0 {
            panic!("end_lexeme called without start_lexeme");
        }

        let result = Span {
            line: self.lexeme_line,
            col: self.lexeme_col,
            lexeme: &self.lexeme_start[..self.lexeme_len],
        };

        self.lexeme_start = self.rest;
        self.lexeme_line = 0;
        self.lexeme_col = 0;
        self.lexeme_len = 0;

        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let span = self.start_lexeme();
            let c = self.advance()?;

            let (kind, span) = match c {
                '(' => (TokenKind::LeftParen, self.end_lexeme()),
                ')' => (TokenKind::RightParen, self.end_lexeme()),
                '{' => (TokenKind::LeftBrace, self.end_lexeme()),
                '}' => (TokenKind::RightBrace, self.end_lexeme()),
                ',' => (TokenKind::Comma, self.end_lexeme()),
                '.' => (TokenKind::Dot, self.end_lexeme()),
                '-' => (TokenKind::Minus, self.end_lexeme()),
                '+' => (TokenKind::Plus, self.end_lexeme()),
                ';' => (TokenKind::Semicolon, self.end_lexeme()),
                '*' => (TokenKind::Star, self.end_lexeme()),
                '=' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        (TokenKind::EqualEqual, self.end_lexeme())
                    } else {
                        (TokenKind::Equal, self.end_lexeme())
                    }
                }
                '!' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        (TokenKind::BangEqual, self.end_lexeme())
                    } else {
                        (TokenKind::Bang, self.end_lexeme())
                    }
                }
                '<' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        (TokenKind::LessEqual, self.end_lexeme())
                    } else {
                        (TokenKind::Less, self.end_lexeme())
                    }
                }
                '>' => {
                    if self.peek() == Some('=') {
                        self.advance();
                        (TokenKind::GreaterEqual, self.end_lexeme())
                    } else {
                        (TokenKind::Greater, self.end_lexeme())
                    }
                }
                '/' => {
                    if self.peek() == Some('/') {
                        while let Some(c) = self.advance() {
                            if c == '\n' {
                                break;
                            }
                        }

                        if self.enable_test_comments {
                            // recognize specially-formatted comments for testing
                            let span = self.end_lexeme();
                            let comment = span.lexeme.trim();
                            if let Some(txt) = comment.strip_prefix("// expect: ") {
                                (TokenKind::ExpectOutput(txt), span)
                            } else if let Some(txt) =
                                comment.strip_prefix("// expect runtime error: ")
                            {
                                (TokenKind::ExpectRuntimeError(txt), span)
                            } else if comment.starts_with("// Error") {
                                let txt = comment.strip_prefix("// ").unwrap();
                                (TokenKind::ExpectParserError(txt), span)
                            } else {
                                // normal comment
                                continue; // ignore, and restart next lexeme
                            }
                        } else {
                            continue; // ignore, and restart next lexeme
                        }
                    } else {
                        (TokenKind::Slash, self.end_lexeme())
                    }
                }
                ' ' | '\t' | '\r' | '\n' => {
                    continue; // ignore, and restart next lexeme
                }
                '"' => loop {
                    let Some(c) = self.advance() else {
                        return Some(Err(Error::unterminated_string(span)));
                    };
                    if c != '"' {
                        continue;
                    }
                    let span = self.end_lexeme();
                    let val = &span.lexeme[1..span.lexeme.len() - 1];
                    break (TokenKind::String(Cow::Borrowed(val)), span);
                },
                '0'..='9' => {
                    while let Some(c) = self.peek() {
                        if !c.is_digit(10) {
                            break;
                        }
                        self.advance();
                    }

                    if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_digit(10))
                    {
                        self.advance(); // dot

                        while let Some(c) = self.peek() {
                            if !c.is_digit(10) {
                                break;
                            }
                            self.advance();
                        }
                    }

                    let span = self.end_lexeme();
                    let val: f64 = match span.lexeme.parse() {
                        Ok(val) => val,
                        Err(_) => {
                            return Some(Err(Error::invalid_number(span)));
                        }
                    };
                    (TokenKind::Number(val), span)
                }
                #[rustfmt::skip]
                'A'..='Z' | 'a'..='z' | '_' => {
                    while let Some(c) = self.peek() {
                        if !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_') { break }
                        self.advance();
                    }
                    let span = self.end_lexeme();

                    let kind = match span.lexeme {
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
                        _           => TokenKind::Identifier(span.lexeme),
                    };

                    (kind, span)
                }
                _ => {
                    return Some(Err(Error::unexpected_character(span)));
                }
            };

            return Some(Ok(Token { kind, span }));
        }
    }
}
