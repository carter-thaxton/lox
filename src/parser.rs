use crate::errors::*;
use crate::lexer::*;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<AstNode, Error> {
        let expr = self.parse_primary()?;

        Ok(AstNode::Expr(expr))
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        if self.matches(Token::Nil).is_some() {
            return Ok(Expr::Literal(Literal::Nil));
        }
        if self.matches(Token::False).is_some() {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.matches(Token::True).is_some() {
            return Ok(Expr::Literal(Literal::True));
        }

        if let Some(Ok(tok)) = self.lexer.peek() {
            match *tok {
                Token::String(val, _) => {
                    self.advance();
                    return Ok(Expr::Literal(Literal::String(val.to_owned())));
                }
                Token::Number(val, _) => {
                    self.advance();
                    return Ok(Expr::Literal(Literal::Number(val)));
                }
                _ => {},
            }
        }

        // TODO: handle other literals and grouping expressions

        unimplemented!("Handle errors")
    }

    fn advance(&mut self) -> Token<'_> {
        self.lexer
            .next()
            .expect("Should not be at EOF")
            .expect("Should not produce a lexer error")
    }

    fn check(&mut self, token: Token<'_>) -> bool {
        if let Some(Ok(tok)) = self.lexer.peek() {
            if *tok == token {
                return true;
            }
        }
        false
    }

    // fn check_p<P>(&mut self, pred: P) -> bool
    // where
    //     P: FnOnce(Token<'_>) -> bool
    // {
    //     if let Some(Ok(tok)) = self.lexer.peek() {
    //         if pred(*tok) {
    //             return true
    //         }
    //     }
    //     false
    // }

    // fn check_n(&mut self, tokens: &[Token<'_>]) -> bool {
    //     if let Some(Ok(tok)) = self.lexer.peek() {
    //         if tokens.contains(tok) {
    //             return true
    //         }
    //     }
    //     false
    // }

    fn matches(&mut self, token: Token<'_>) -> Option<Token<'_>> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    // fn matches_p<P>(&mut self, pred: P) -> Option<Token<'_>>
    // where
    //     P: FnOnce(Token<'_>) -> bool
    // {
    //     if self.check_p(pred) {
    //         Some(self.advance())
    //     } else {
    //         None
    //     }
    // }
}

pub enum AstNode {
    Expr(Expr),
}

pub enum Expr {
    Literal(Literal),
    UnaryExpr {
        op: Op,
        right: Box<Expr>,
    },
    BinaryExpr {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Group(Box<Expr>), // needed only to print out expected format
}

pub enum Literal {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
}

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            AstNode::Expr(expr) => expr.fmt(f),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            _ => {
                unimplemented!()
            }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Literal::Nil => {
                write!(f, "nil")
            }
            Literal::True => {
                write!(f, "true")
            }
            Literal::False => {
                write!(f, "false")
            }
            Literal::Number(n) => {
                if *n == n.trunc() && !n.is_infinite() && !n.is_nan() {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Literal::String(s) => {
                write!(f, "{}", s)
            }
        }
    }
}
