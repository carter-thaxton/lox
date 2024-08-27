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
        let expr = self.parse_expr()?;

        Ok(AstNode::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_comparison()?;

        while let Some(tok) = self.matches_n(&[Token::EqualEqual, Token::BangEqual]) {
            let op = match tok {
                Token::EqualEqual => Op::Eq,
                Token::BangEqual => Op::Ne,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            left = Expr::BinaryExpr { op, left: Box::new(left), right: Box::new(right) };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_term()?;

        while let Some(tok) = self.matches_n(&[Token::Less, Token::LessEqual, Token::Greater, Token::GreaterEqual]) {
            let op = match tok {
                Token::Less => Op::Lt,
                Token::LessEqual => Op::Le,
                Token::Greater => Op::Gt,
                Token::GreaterEqual => Op::Ge,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            left = Expr::BinaryExpr { op, left: Box::new(left), right: Box::new(right) };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_factor()?;

        while let Some(tok) = self.matches_n(&[Token::Plus, Token::Minus]) {
            let op = match tok {
                Token::Plus => Op::Add,
                Token::Minus => Op::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            left = Expr::BinaryExpr { op, left: Box::new(left), right: Box::new(right) };
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_unary()?;

        while let Some(tok) = self.matches_n(&[Token::Star, Token::Slash]) {
            let op = match tok {
                Token::Star => Op::Mul,
                Token::Slash => Op::Div,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            left = Expr::BinaryExpr { op, left: Box::new(left), right: Box::new(right) };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, Error> {
        if self.matches(Token::Minus).is_some() {
            let right = self.parse_unary()?;
            return Ok(Expr::UnaryExpr { op: Op::Neg, right: Box::new(right) });
        }
        if self.matches(Token::Bang).is_some() {
            let right = self.parse_unary()?;
            return Ok(Expr::UnaryExpr { op: Op::Not, right: Box::new(right) });
        }

        self.parse_primary()
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

        if let Some(Ok(tok)) = self.peek() {
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

        if self.matches(Token::LeftParen).is_some() {
            let expr = self.parse_expr()?;
            self.expect(Token::RightParen)?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        Err(self.parse_error("Expect expression."))
    }

    fn parse_error(&mut self, message: &str) -> Error {
        let line = self.peek_line().expect("Should not be at EOF");
        let next_token = match self.peek() {
            Some(Ok(tok)) => Some(tok),
            _ => None,
        };
        Error::parser_error(line, next_token, message)
    }

    fn advance(&mut self) -> Token<'_> {
        self.lexer
            .next()
            .expect("Should not be at EOF")
            .expect("Should not produce a lexer error")
            .0
    }

    fn peek(&mut self) -> Option<Result<&Token<'a>, &Error>> {
        match self.lexer.peek() {
            Some(Ok((token, _line))) => Some(Ok(token)),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }

    fn peek_line(&mut self) -> Option<usize> {
        match self.lexer.peek() {
            Some(Ok((_token, line))) => Some(*line),
            Some(Err(_err)) => None,
            None => None,
        }
    }

    fn check(&mut self, token: Token<'_>) -> bool {
        if let Some(Ok(tok)) = self.peek() {
            if *tok == token {
                return true;
            }
        }
        false
    }

    fn check_n(&mut self, tokens: &[Token<'_>]) -> bool {
        if let Some(Ok(tok)) = self.peek() {
            if tokens.contains(tok) {
                return true
            }
        }
        false
    }

    fn matches(&mut self, token: Token<'_>) -> Option<Token<'_>> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn matches_n(&mut self, tokens: &[Token<'_>]) -> Option<Token<'_>> {
        if self.check_n(tokens) {
            Some(self.advance())
        } else {
            None
        }
    }

    // fn check_p<P>(&mut self, pred: P) -> bool
    // where
    //     P: FnOnce(Token<'_>) -> bool
    // {
    //     if let Some(Ok(tok)) = self.peek() {
    //         if pred(*tok) {
    //             return true
    //         }
    //     }
    //     false
    // }

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

    fn expect(&mut self, token: Token<'_>) -> Result<(), Error> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            unimplemented!("Handle errors");
        }
    }
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
    Neg,
    Not,
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
            Expr::Group(expr) => write!(f, "(group {})", expr),
            Expr::UnaryExpr { op, right } => write!(f, "({} {})", op, right),
            Expr::BinaryExpr { op, left, right } => write!(f, "({} {} {})", op, left, right),
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
                write!(f, "{}", s)  // very bad format for literals.  should be quoted ""
            }
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let s = match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Not => "!",
            Op::Neg => "-",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
        };
        write!(f, "{}", s)
    }
}
