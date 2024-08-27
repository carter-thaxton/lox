use std::iter::Peekable;
use crate::errors::*;
use crate::lexer::*;
use std::fmt::{Display, Formatter};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input).peekable()
        }
    }

    pub fn parse(mut self) -> Result<AstNode, Error> {
        let expr = self.parse_primary()?;

        Ok(AstNode::Expr(expr))
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        if self.check(Token::Nil) {
            return Ok(Expr::Literal(Literal::Nil));
        }
        if self.check(Token::False) {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.check(Token::True) {
            return Ok(Expr::Literal(Literal::True));
        }

        // TODO: handle other literals and grouping expressions

        unimplemented!("Handle errors")
    }

    fn check(&mut self, token: Token<'_>) -> bool {
        if let Some(Ok(tok)) = self.lexer.peek() {
            if *tok == token {
                return true
            }
        }
        false
    }

    // fn check_n(&mut self, tokens: &[Token<'_>]) -> bool {
    //     if let Some(Ok(tok)) = self.lexer.peek() {
    //         if tokens.contains(tok) {
    //             return true
    //         }
    //     }
    //     false
    // }
}


pub enum AstNode {
    Expr(Expr),
}

pub enum Expr {
    Literal(Literal),
    UnaryExpr { op: Op, right: Box<Expr> },
    BinaryExpr { op: Op, left: Box<Expr>, right: Box<Expr> },
    Group(Box<Expr>),  // needed only to print out expected format
}

pub enum Literal {
    Nil,
    True,
    False,
    Number,
    String,
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
            AstNode::Expr(expr) => {
                expr.fmt(f)
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Literal(literal) => {
                literal.fmt(f)
            }
            _ => { unimplemented!() }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Literal::Nil => { write!(f, "nil") }
            Literal::True => { write!(f, "true") }
            Literal::False => { write!(f, "false") }
            Literal::Number => { write!(f, "123") }
            Literal::String => { write!(f, "\"str\"") }
        }
    }
}
