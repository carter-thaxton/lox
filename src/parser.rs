use crate::ast::*;
use crate::errors::*;
use crate::lexer::*;
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    last_line: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, enable_test_comments: bool) -> Self {
        Parser {
            lexer: Lexer::new(input, enable_test_comments).peekable(),
            last_line: 1,
        }
    }

    pub fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub fn parse(&mut self) -> Result<Program, Error<'a>> {
        let mut program = Vec::new();

        while !self.at_eof() {
            let stmt = self.parse_declaration()?;
            program.push(stmt);
        }

        Ok(program)
    }

    pub fn lexer(&mut self) -> &mut Peekable<Lexer<'a>> {
        &mut self.lexer
    }

    //
    // statements
    //

    fn parse_declaration(&mut self) -> Result<Stmt, Error<'a>> {
        // var <name> (= <expr>)? ;
        if self.matches(TokenKind::Var).is_some() {
            let name = self
                .consume_identifier("Expect variable name.")?
                .to_string();

            if self.matches(TokenKind::Equal).is_some() {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
                return Ok(Stmt::Var(name, Some(expr)));
            } else {
                self.consume(TokenKind::Semicolon, "Expect ';' after var.")?;
                return Ok(Stmt::Var(name, None));
            }
        }

        self.parse_stmt()
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error<'a>> {
        // == TEST ==
        // expect: <output value>
        // expect runtime error: <error message>
        // Error <parser error>
        while let Some(tok) = self.matches_p(|t| {
            matches!(
                t,
                TokenKind::ExpectOutput(_)
                    | TokenKind::ExpectParserError(_)
                    | TokenKind::ExpectRuntimeError(_)
            )
        }) {
            match tok.kind {
                TokenKind::ExpectOutput(txt) => return Ok(Stmt::ExpectOutput(txt.to_string())),
                TokenKind::ExpectRuntimeError(msg) => return Ok(Stmt::ExpectRuntimeError(msg.to_string())),
                TokenKind::ExpectParserError(_msg) => {
                    // ignore this while parsing...
                    continue;
                }
                _ => unreachable!(),
            }
        }

        // if (<cond>) <then> ( else <else> )?
        if self.matches(TokenKind::If).is_some() {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.")?;
            let cond = self.parse_expr()?;
            self.consume(TokenKind::RightParen, "Expect ')' after if condition.")?;

            let then_branch = self.parse_stmt()?;
            let else_branch = if self.matches(TokenKind::Else).is_some() {
                Some(self.parse_stmt()?)
            } else {
                None
            };

            let cond = Box::new(cond);
            let then_branch = Box::new(then_branch);
            let else_branch = else_branch.map(|e| Box::new(e));

            return Ok(Stmt::IfElse(cond, then_branch, else_branch));
        }

        // { (<stmt>)* }
        if self.matches(TokenKind::LeftBrace).is_some() {
            let mut stmts = Vec::new();
            while !self.at_eof() {
                if self.matches(TokenKind::RightBrace).is_some() {
                    return Ok(Stmt::Block(stmts));
                }
                let stmt = self.parse_declaration()?;
                stmts.push(stmt);
            }
            return Err(self.parser_error("Expect '}' after block."));
        }

        // print <expr> ;
        if self.matches(TokenKind::Print).is_some() {
            let expr = self.parse_expr()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
            return Ok(Stmt::Print(Box::new(expr)));
        }

        // <expr> ;
        let expr = self.parse_expr()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after expresssion.")?;
        Ok(Stmt::Expr(Box::new(expr)))
    }

    //
    // expressions
    //

    pub fn parse_expr(&mut self) -> Result<Expr, Error<'a>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, Error<'a>> {
        let left = self.parse_logical_or()?;

        // <left> = <right> ;
        if self.matches(TokenKind::Equal).is_some() {
            // <left> must be an L-value
            if let Expr::Variable(name) = left {
                let right = self.parse_assignment()?;
                return Ok(Expr::Assign {
                    name,
                    right: Box::new(right),
                });
            } else {
                return Err(self.parser_error("Invalid assignment target."));
            }
        }

        Ok(left)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_logical_and()?;

        // <left> or <right>
        while self.matches(TokenKind::Or).is_some() {
            let right = self.parse_logical_and()?;
            left = Expr::BinaryExpr {
                op: Op::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_equality()?;

        // <left> and <right>
        while self.matches(TokenKind::And).is_some() {
            let right = self.parse_equality()?;
            left = Expr::BinaryExpr {
                op: Op::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, Error<'a>> {
        let mut left = self.parse_comparison()?;

        // <left> == <right> | <left> != <right>
        while let Some(tok) = self.matches_n(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let op = match tok.kind {
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

        // <left> < <right> | <left> <= <right> | <left> > <right> | <left> >= <right>
        while let Some(tok) = self.matches_n(&[
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::Greater,
            TokenKind::GreaterEqual,
        ]) {
            let op = match tok.kind {
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

        // <left> + <right> | <left> - <right>
        while let Some(tok) = self.matches_n(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match tok.kind {
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

        // <left> * <right> | <left> / <right>
        while let Some(tok) = self.matches_n(&[TokenKind::Star, TokenKind::Slash]) {
            let op = match tok.kind {
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
        // - <right>
        if self.matches(TokenKind::Minus).is_some() {
            let right = self.parse_unary()?;
            return Ok(Expr::UnaryExpr {
                op: Op::Neg,
                right: Box::new(right),
            });
        }

        // ! <right>
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
        // nil
        if self.matches(TokenKind::Nil).is_some() {
            return Ok(Expr::Literal(Literal::Nil));
        }

        // false
        if self.matches(TokenKind::False).is_some() {
            return Ok(Expr::Literal(Literal::False));
        }

        // true
        if self.matches(TokenKind::True).is_some() {
            return Ok(Expr::Literal(Literal::True));
        }

        // "<str>"
        if let Some(tok) = self.matches_p(|t| matches!(t, TokenKind::String(_))) {
            match tok.kind {
                TokenKind::String(val) => {
                    return Ok(Expr::Literal(Literal::String(val.to_string())))
                }
                _ => unreachable!(),
            }
        }

        // <num>
        if let Some(tok) = self.matches_p(|t| matches!(t, TokenKind::Number(_))) {
            match tok.kind {
                TokenKind::Number(val) => return Ok(Expr::Literal(Literal::Number(val))),
                _ => unreachable!(),
            }
        }

        // <identifier>
        if let Some(name) = self.matches_identifier() {
            return Ok(Expr::Variable(name.to_string()));
        }

        // ( <expr> )
        if self.matches(TokenKind::LeftParen).is_some() {
            let expr = self.parse_expr()?;
            self.consume(TokenKind::RightParen, "Expect closing ')'.")?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        self.matches_err()?;
        Err(self.parser_error("Expect expression."))
    }

    //
    // helpers
    //

    // peek ahead without advancing
    // returns None at EOF, an Err if a lexing error occurs, and the next TokenKind otherwise
    fn peek(&mut self) -> Option<Result<&TokenKind<'a>, &Error<'a>>> {
        match self.lexer.peek() {
            Some(Ok(token)) => Some(Ok(&token.kind)),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }

    // peek ahead without advancing to the next available token span, useful for reporting parser errors
    // returns None at EOF
    fn peek_span(&mut self) -> Option<&Span<'a>> {
        match self.lexer.peek() {
            Some(Ok(token)) => Some(&token.span),
            Some(Err(Error {
                span: Some(span), ..
            })) => Some(span),
            _ => None,
        }
    }

    // move to the next token, returning the token
    // panics if at EOF or if a lexing error occurs - should use the various peek and check methods first, to be sure it will succeed
    // keeps track of last-seen line number, for errors at EOF
    fn advance(&mut self) -> Token<'a> {
        let token = self
            .lexer
            .next()
            .expect("Should not be at EOF")
            .expect("Should not produce a lexer error");

        self.last_line = token.span.line;

        token
    }

    // peeks ahead without advancing, to see if the next token matches the given kind
    fn check(&mut self, token: TokenKind<'_>) -> bool {
        if let Some(Ok(kind)) = self.peek() {
            if *kind == token {
                return true;
            }
        }
        false
    }

    // like check, but advances when successful, returning the whole token if it does
    fn matches(&mut self, token: TokenKind<'_>) -> Option<Token<'_>> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    // check to see if next token is a lexer error, and advances if so
    // does nothing at EOF or when next token is not an error
    fn matches_err(&mut self) -> Result<(), Error<'a>> {
        match self.lexer.peek() {
            Some(Err(_)) => {
                Err(self.lexer.next().unwrap().unwrap_err())
            }
            _ => Ok(()),
        }
    }

    // create a parser error referring to the span of the next token, with the given message to match the book
    // at EOF, constructs a dummy span using the last seen line number
    fn parser_error(&mut self, message: impl Into<String>) -> Error<'a> {
        if let Some(span) = self.peek_span() {
            Error::parser_error(*span, message)
        } else {
            Error::parser_error(Span::dummy_for_line(self.last_line), message)
        }
    }

    // like matches, but produces a parser error if it doesn't match
    fn consume(&mut self, token: TokenKind<'_>, message: &str) -> Result<Token<'a>, Error<'a>> {
        if self.check(token) {
            Ok(self.advance())
        } else {
            Err(self.parser_error(message))
        }
    }

    // versions of the above, for multiple TokenKinds, or for arbitrary predicates

    fn check_n(&mut self, tokens: &[TokenKind<'_>]) -> bool {
        if let Some(Ok(kind)) = self.peek() {
            if tokens.contains(kind) {
                return true;
            }
        }
        false
    }

    fn matches_n(&mut self, tokens: &[TokenKind<'_>]) -> Option<Token<'_>> {
        if self.check_n(tokens) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn check_p<P>(&mut self, pred: P) -> bool
    where
        P: FnOnce(&TokenKind<'_>) -> bool,
    {
        if let Some(Ok(kind)) = self.peek() {
            if pred(kind) {
                return true;
            }
        }
        false
    }

    fn matches_p<P>(&mut self, pred: P) -> Option<Token<'_>>
    where
        P: FnOnce(&TokenKind<'_>) -> bool,
    {
        if self.check_p(pred) {
            Some(self.advance())
        } else {
            None
        }
    }

    // fn consume_p<P>(&mut self, pred: P, message: &str) -> Result<Token<'a>, Error<'a>>
    // where
    //     P: FnOnce(&TokenKind<'_>) -> bool
    // {
    //     if self.check_p(pred) {
    //         Ok(self.advance())
    //     } else {
    //         Err(self.parser_error(message))
    //     }
    // }

    // similar to above, but handles extracting the string from an identifier

    fn check_identifier(&mut self) -> bool {
        self.check_p(|kind| matches!(kind, TokenKind::Identifier(_)))
    }

    fn matches_identifier(&mut self) -> Option<&'a str> {
        if self.check_identifier() {
            match self.advance().kind {
                TokenKind::Identifier(name) => Some(name),
                _ => unreachable!("Known to be identifier"),
            }
        } else {
            None
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<&'a str, Error<'a>> {
        if let Some(name) = self.matches_identifier() {
            Ok(name)
        } else {
            Err(self.parser_error(message))
        }
    }
}
