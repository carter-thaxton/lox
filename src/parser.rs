use crate::ast::*;
use crate::errors::*;
use crate::lexer::*;
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

    pub fn parse(mut self) -> Result<AstNode, Error<'a>> {
        let expr = self.parse_expr()?;
        Ok(AstNode::Expr(expr))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, Error<'a>> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_comparison()?;

        while let Some(tok) = self.matches_n(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = match tok {
                TokenKind::EqualEqual => Op::Eq,
                TokenKind::BangEqual => Op::Ne,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            left = Expr::BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_term()?;

        while let Some(tok) = self.matches_n(&[
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::Greater,
            TokenKind::GreaterEqual,
        ]) {
            let op = match tok {
                TokenKind::Less => Op::Lt,
                TokenKind::LessEqual => Op::Le,
                TokenKind::Greater => Op::Gt,
                TokenKind::GreaterEqual => Op::Ge,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            left = Expr::BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_factor()?;

        while let Some(tok) = self.matches_n(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match tok {
                TokenKind::Plus => Op::Add,
                TokenKind::Minus => Op::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            left = Expr::BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_unary()?;

        while let Some(tok) = self.matches_n(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match tok {
                TokenKind::Star => Op::Mul,
                TokenKind::Slash => Op::Div,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            left = Expr::BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, Error<'a>> {
        if self.matches(TokenKind::Minus).is_some() {
            let right = self.parse_unary()?;
            return Ok(Expr::UnaryExpr {
                op: Op::Neg,
                right: Box::new(right),
            });
        }
        if self.matches(TokenKind::Bang).is_some() {
            let right = self.parse_unary()?;
            return Ok(Expr::UnaryExpr {
                op: Op::Not,
                right: Box::new(right),
            });
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, Error<'a>> {
        if self.matches(TokenKind::Nil).is_some() {
            return Ok(Expr::Literal(Literal::Nil));
        }
        if self.matches(TokenKind::False).is_some() {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.matches(TokenKind::True).is_some() {
            return Ok(Expr::Literal(Literal::True));
        }

        if let Some(Ok(tok)) = self.peek() {
            match tok.clone() {
                // TODO: can we avoid this?
                TokenKind::String(val) => {
                    self.advance();
                    return Ok(Expr::Literal(Literal::String(val.to_string())));
                }
                TokenKind::Number(val) => {
                    self.advance();
                    return Ok(Expr::Literal(Literal::Number(val)));
                }
                _ => {}
            }
        }

        if self.matches(TokenKind::LeftParen).is_some() {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::RightParen)?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        Err(self.parser_error("Expect expression."))
    }

    fn parser_error(&mut self, message: &str) -> Error<'a> {
        let span = self.peek_span().expect("Should not be at EOF").clone();
        Error::parser_error(span, message)
    }

    fn advance(&mut self) -> TokenKind<'_> {
        self.lexer
            .next()
            .expect("Should not be at EOF")
            .expect("Should not produce a lexer error")
            .kind
    }

    fn peek(&mut self) -> Option<Result<&TokenKind<'a>, &Error<'a>>> {
        match self.lexer.peek() {
            Some(Ok(token)) => Some(Ok(&token.kind)),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }

    fn peek_span(&mut self) -> Option<&Span<'a>> {
        match self.lexer.peek() {
            Some(Ok(token)) => Some(&token.span),
            Some(Err(Error { span: Some(span), .. })) => Some(&span),
            _ => None,
        }
    }

    fn check(&mut self, token: TokenKind<'_>) -> bool {
        if let Some(Ok(tok)) = self.peek() {
            if *tok == token {
                return true;
            }
        }
        false
    }

    fn check_n(&mut self, tokens: &[TokenKind<'_>]) -> bool {
        if let Some(Ok(tok)) = self.peek() {
            if tokens.contains(tok) {
                return true;
            }
        }
        false
    }

    fn matches(&mut self, token: TokenKind<'_>) -> Option<TokenKind<'_>> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn matches_n(&mut self, tokens: &[TokenKind<'_>]) -> Option<TokenKind<'_>> {
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

    fn expect(&mut self, token: TokenKind<'_>) -> Result<(), Error<'a>> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            unimplemented!("Handle errors");
        }
    }
}
