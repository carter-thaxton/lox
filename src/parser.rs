use crate::ast::*;
use crate::errors::*;
use crate::lexer::*;
use crate::resolver::resolve;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

enum FunctionKind {
    Function,
    Method,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            FunctionKind::Function => write!(f, "function"),
            FunctionKind::Method => write!(f, "method"),
        }
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    last_line: usize,
}

#[derive(Clone, Copy)]
struct LoopContext<'a> {
    post_incr: Option<&'a Expr>,
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

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut program = Vec::new();

        // first pass - parse
        while !self.at_eof() {
            let stmt = self.parse_declaration(false, None)?;
            program.push(stmt);
        }

        // second pass - resolve declarations
        resolve(&mut program)?;

        Ok(program)
    }

    pub fn lexer(&mut self) -> &mut Peekable<Lexer<'a>> {
        &mut self.lexer
    }

    //
    // statements
    //

    fn parse_declaration(
        &mut self,
        in_function: bool,
        in_loop: Option<LoopContext>,
    ) -> Result<Stmt, Error> {
        if let Some(stmt) = self.parse_test_comments() {
            return Ok(stmt);
        }

        // var <name> (= <expr>)? ;
        if self.matches(TokenKind::Var).is_some() {
            return self.parse_var_decl();
        }

        // class <name> { ( <method> )* }
        if self.matches(TokenKind::Class).is_some() {
            let (name, tok) = self.consume_identifier("Expect class name.")?;
            self.consume(TokenKind::LeftBrace, "Expect '{' before class body.")?;

            let mut methods: Vec<Stmt> = vec![];
            while !self.at_eof() && !self.check(TokenKind::RightBrace) {
                let method = self.parse_fun_decl(FunctionKind::Method)?;
                methods.push(method);
            }

            self.consume(TokenKind::RightBrace, "Expect '}' after class body.")?;

            return Ok(Stmt::Class { name: name.to_string(), methods, line: tok.span.line });
        }

        // fun <name> ( (<arg>, )* ) { <body> }
        // explicitly look ahead two tokens, for 'fun <name>', to allow for anonymous 'fun' expressions
        if self.check2_p(|t1, t2| *t1 == TokenKind::Fun && matches!(t2, TokenKind::Identifier(_))) {
            self.consume(
                TokenKind::Fun,
                "Expect 'fun' to begin function declaration.",
            )?;
            return self.parse_fun_decl(FunctionKind::Function);
        }

        self.parse_stmt(in_function, in_loop)
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, Error> {
        let (name, tok) = self.consume_identifier("Expect variable name.")?;

        if let Some(tok) = self.matches(TokenKind::Equal) {
            let expr = self.parse_expr()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
            return Ok(Stmt::Var {
                name: name.to_string(),
                init: Some(expr),
                line: tok.span.line,
            });
        } else {
            self.consume(TokenKind::Semicolon, "Expect ';' after var.")?;
            return Ok(Stmt::Var { name: name.to_string(), init: None, line: tok.span.line });
        }
    }

    fn parse_fun_decl(&mut self, kind: FunctionKind) -> Result<Stmt, Error> {
        let (name, _tok) = self.consume_identifier(format!("Expect {} name.", kind))?;
        let lparen = self.consume(
            TokenKind::LeftParen,
            format!("Expect '(' after {} name.", kind),
        )?;
        let line = lparen.span.line; // use line number of opening parenthesis

        let mut params: Vec<(String, usize)> = vec![];
        if !self.check(TokenKind::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(self.parser_error("Can't have more than 255 parameters."));
                }
                let (param, tok) = self.consume_identifier("Expect parameter name.")?;
                params.push((param.to_string(), tok.span.line));
                if !self.matches(TokenKind::Comma).is_some() {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            TokenKind::LeftBrace,
            format!("Expect '{{' before {} body.", kind),
        )?;

        let body = self.parse_block(true, None)?;

        return Ok(Stmt::Function {
            name: name.to_string(),
            params,
            body,
            line,
        });
    }

    fn parse_stmt(
        &mut self,
        in_function: bool,
        in_loop: Option<LoopContext>,
    ) -> Result<Stmt, Error> {
        // ;
        while self.matches(TokenKind::Semicolon).is_some() {
            // simply ignore empty statement
        }

        // expect ...
        if let Some(stmt) = self.parse_test_comments() {
            return Ok(stmt);
        }

        // if (<cond>) <then> ( else <else> )?
        if self.matches(TokenKind::If).is_some() {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.")?;
            let cond = self.parse_expr()?;
            self.consume(TokenKind::RightParen, "Expect ')' after if condition.")?;

            let then_branch = self.parse_stmt(in_function, in_loop)?;
            let else_branch = if self.matches(TokenKind::Else).is_some() {
                Some(self.parse_stmt(in_function, in_loop)?)
            } else {
                None
            };

            let cond = Box::new(cond);
            let then_branch = Box::new(then_branch);
            let else_branch = else_branch.map(|e| Box::new(e));

            return Ok(Stmt::IfElse {
                cond,
                then_branch,
                else_branch,
            });
        }

        // while (<cond>) <body>
        if self.matches(TokenKind::While).is_some() {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'while'.")?;
            let cond = self.parse_expr()?;
            self.consume(TokenKind::RightParen, "Expect ')' after condition.")?;

            let loop_context = LoopContext { post_incr: None };
            let body = self.parse_stmt(in_function, Some(loop_context))?;

            return Ok(Stmt::While {
                cond: Box::new(cond),
                body: Box::new(body),
            });
        }

        // for ( (<init>)? ; (<cond>)> ; (<incr>)? ) <body>
        if self.matches(TokenKind::For).is_some() {
            self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.")?;

            let init = if self.matches(TokenKind::Semicolon).is_some() {
                None
            } else if self.matches(TokenKind::Var).is_some() {
                Some(self.parse_var_decl()?)
            } else {
                Some(self.parse_expr_stmt()?)
            };

            let cond = if self.matches(TokenKind::Semicolon).is_some() {
                None
            } else {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::Semicolon, "Expect ';' after loop condition.")?;
                Some(expr)
            };

            let incr = if self.check(TokenKind::RightParen) {
                None
            } else {
                Some(self.parse_expr()?)
            };

            self.consume(TokenKind::RightParen, "Expect ')' after for clauses.")?;

            // parse body, with an implied post-increment expression, which should be
            // inserted before any 'continue' statements in the loop.
            let loop_context = LoopContext {
                post_incr: incr.as_ref(),
            };
            let body = self.parse_stmt(in_function, Some(loop_context))?;

            // desugar to while-loop:
            // {
            //   <init>
            //   while (<cond>) {
            //     <body>
            //     <incr>
            //   }
            // }

            let body_with_incr = if let Some(incr) = incr {
                Stmt::Block(vec![body, Stmt::Expr(Box::new(incr))])
            } else {
                body
            };

            let cond = cond.unwrap_or(Expr::Literal(Literal::True));

            let body_with_while = Stmt::While {
                cond: Box::new(cond),
                body: Box::new(body_with_incr),
            };

            let body_with_init_and_while = if let Some(init) = init {
                Stmt::Block(vec![init, body_with_while])
            } else {
                body_with_while
            };

            return Ok(body_with_init_and_while);
        }

        // { (<stmt>)* }
        if self.matches(TokenKind::LeftBrace).is_some() {
            let stmts = self.parse_block(in_function, in_loop)?;
            return Ok(Stmt::Block(stmts));
        }

        // print <expr> ;
        if self.matches(TokenKind::Print).is_some() {
            let expr = self.parse_expr()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
            return Ok(Stmt::Print(Box::new(expr)));
        }

        // return ( <expr> )? ;
        if let Some(tok) = self.matches(TokenKind::Return) {
            if !in_function {
                return Err(Error::parser_error(
                    tok.span,
                    "Can't return from top-level code.",
                ));
            }
            let expr = if !self.check(TokenKind::Semicolon) {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };
            self.consume(TokenKind::Semicolon, "Expect ';' after return value.")?;
            return Ok(Stmt::Return(expr));
        }

        // break ;
        if let Some(tok) = self.matches(TokenKind::Break) {
            if !in_loop.is_some() {
                return Err(Error::parser_error(tok.span, "Can only break within loop."));
            }
            self.consume(TokenKind::Semicolon, "Expect ';' after break.")?;
            return Ok(Stmt::Break);
        }

        // continue ;
        if let Some(tok) = self.matches(TokenKind::Continue) {
            if let Some(loop_context) = in_loop {
                self.consume(TokenKind::Semicolon, "Expect ';' after continue.")?;
                if let Some(incr) = loop_context.post_incr {
                    // if loop-context includes a post-increment expression, then execute it before continuing
                    let incr = Stmt::Expr(Box::new(incr.clone()));
                    return Ok(Stmt::Block(vec![incr, Stmt::Continue]));
                } else {
                    return Ok(Stmt::Continue);
                }
            } else {
                return Err(Error::parser_error(
                    tok.span,
                    "Can only continue within loop.",
                ));
            }
        }

        // <expr> ;
        self.parse_expr_stmt()
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.parse_expr()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after expresssion.")?;
        Ok(Stmt::Expr(Box::new(expr)))
    }

    fn parse_block(
        &mut self,
        in_function: bool,
        in_loop: Option<LoopContext>,
    ) -> Result<Vec<Stmt>, Error> {
        let mut stmts = Vec::new();
        while !self.at_eof() {
            if let Some(stmt) = self.parse_test_comments() {
                stmts.push(stmt);
            }
            if self.matches(TokenKind::RightBrace).is_some() {
                return Ok(stmts);
            }
            let stmt = self.parse_declaration(in_function, in_loop)?;
            stmts.push(stmt);
        }
        return Err(self.parser_error("Expect '}' after block."));
    }

    fn parse_test_comments(&mut self) -> Option<Stmt> {
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
                TokenKind::ExpectOutput(txt) => return Some(Stmt::ExpectOutput(txt.to_string())),
                TokenKind::ExpectRuntimeError(msg) => {
                    return Some(Stmt::ExpectRuntimeError(msg.to_string()))
                }
                TokenKind::ExpectParserError(_msg) => {
                    // ignore this while parsing...
                    continue;
                }
                _ => unreachable!(),
            }
        }
        None
    }

    //
    // expressions
    //

    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, Error> {
        let left = self.parse_logical_or()?;

        // <left> = <right> ;
        if let Some(tok) = self.matches(TokenKind::Equal) {
            // <left> must be an L-value
            if let Expr::Variable { name, .. } = left {
                let right = self.parse_assignment()?;
                return Ok(Expr::Assign {
                    name,
                    right: Box::new(right),
                    line: tok.span.line,
                    depth_and_index: None,
                });
            } else {
                // return error referring to '='
                return Err(Error::parser_error(tok.span, "Invalid assignment target."));
            }
        }

        Ok(left)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, Error> {
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

    fn parse_logical_and(&mut self) -> Result<Expr, Error> {
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

    fn parse_equality(&mut self) -> Result<Expr, Error> {
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

    fn parse_comparison(&mut self) -> Result<Expr, Error> {
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

    fn parse_term(&mut self) -> Result<Expr, Error> {
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

    fn parse_factor(&mut self) -> Result<Expr, Error> {
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

    fn parse_unary(&mut self) -> Result<Expr, Error> {
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

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_primary()?;

        loop {
            // <callee> ( <arg1>, .., <argN> )
            if self.matches(TokenKind::LeftParen).is_some() {
                let mut args: Vec<Expr> = vec![];
                if !self.check(TokenKind::RightParen) {
                    loop {
                        if args.len() >= 255 {
                            return Err(self.parser_error("Can't have more than 255 arguments."));
                        }
                        args.push(self.parse_expr()?);
                        if !self.matches(TokenKind::Comma).is_some() {
                            break;
                        }
                    }
                }

                let closing_paren =
                    self.consume(TokenKind::RightParen, "Expect ')' after arguments.")?;

                left = Expr::Call {
                    callee: Box::new(left),
                    args,
                    line: closing_paren.span.line,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
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
        if let Some((name, tok)) = self.matches_identifier() {
            return Ok(Expr::Variable {
                name: name.to_string(),
                depth_and_index: None,
                line: tok.span.line,
            });
        }

        // ( <expr> )
        if self.matches(TokenKind::LeftParen).is_some() {
            let expr = self.parse_expr()?;
            self.consume(TokenKind::RightParen, "Expect closing ')'.")?;
            return Ok(Expr::Group(Box::new(expr)));
        }

        // fun ( (<arg>, )* ) { <body> }
        if self.check2(TokenKind::Fun, TokenKind::LeftParen) {
            return self.parse_fun_expr();
        }

        self.matches_err()?;
        Err(self.parser_error("Expect expression."))
    }

    // fun ( (<arg>, )* ) { <body> }
    fn parse_fun_expr(&mut self) -> Result<Expr, Error> {
        self.consume(TokenKind::Fun, "Expect 'fun' to start anonymous function.")?;

        let lparen = self.consume(TokenKind::LeftParen, format!("Expect '(' after fun."))?;
        let line = lparen.span.line; // use line number of opening parenthesis

        let mut params: Vec<(String, usize)> = vec![];
        if !self.check(TokenKind::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(self.parser_error("Can't have more than 255 parameters."));
                }
                let (param, tok) = self.consume_identifier("Expect parameter name.")?;
                params.push((param.to_string(), tok.span.line));
                if !self.matches(TokenKind::Comma).is_some() {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            TokenKind::LeftBrace,
            format!("Expect '{{' before function body."),
        )?;

        let body = self.parse_block(true, None)?;

        return Ok(Expr::Function { params, body, line });
    }

    //
    // helpers
    //

    // peek ahead without advancing
    // returns None at EOF, an Err if a lexing error occurs, and the next TokenKind otherwise
    fn peek(&mut self) -> Option<Result<&TokenKind<'a>, &Error>> {
        match self.lexer.peek() {
            Some(Ok(token)) => Some(Ok(&token.kind)),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }

    // advance ahead to next token, skipping over any test comments, useful for reporting parser errors
    // returns None at EOF
    fn next_span_for_error(&mut self) -> Option<ErrorSpan> {
        loop {
            match self.lexer.next() {
                Some(Ok(token)) if token.kind.is_test() => {
                    continue;
                }
                Some(Ok(token)) if !token.kind.is_test() => {
                    return Some(token.span.into());
                }
                Some(Err(Error {
                    span: Some(span), ..
                })) => {
                    return Some(span);
                }
                _ => {
                    return None;
                }
            }
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
    fn check(&mut self, token: TokenKind<'a>) -> bool {
        if let Some(Ok(kind)) = self.peek() {
            if *kind == token {
                return true;
            }
        }
        false
    }

    // like check, but advances when successful, returning the whole token if it does
    fn matches(&mut self, token: TokenKind<'a>) -> Option<Token<'a>> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    // check to see if next token is a lexer error, and advances if so
    // does nothing at EOF or when next token is not an error
    fn matches_err(&mut self) -> Result<(), Error> {
        match self.lexer.peek() {
            Some(Err(_)) => Err(self.lexer.next().unwrap().unwrap_err()),
            _ => Ok(()),
        }
    }

    // create a parser error referring to the span of the next token, with the given message to match the book
    // at EOF, uses the last seen line number
    fn parser_error(&mut self, message: impl Into<String>) -> Error {
        if let Some(span) = self.next_span_for_error() {
            Error::parser_error(span, message)
        } else {
            Error::parser_error_on_line(self.last_line, message)
        }
    }

    // like matches, but produces a parser error if it doesn't match
    fn consume(
        &mut self,
        token: TokenKind<'a>,
        message: impl Into<String>,
    ) -> Result<Token<'a>, Error> {
        if self.check(token) {
            Ok(self.advance())
        } else {
            self.matches_err()?;
            Err(self.parser_error(message))
        }
    }

    // versions of the above, for multiple TokenKinds, or for arbitrary predicates

    fn check_n(&mut self, tokens: &[TokenKind<'a>]) -> bool {
        if let Some(Ok(kind)) = self.peek() {
            if tokens.contains(kind) {
                return true;
            }
        }
        false
    }

    fn matches_n(&mut self, tokens: &[TokenKind<'a>]) -> Option<Token<'_>> {
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

    // fn consume_p<P>(&mut self, pred: P, message: impl Into<String>) -> Result<Token<'a>, Error<'a>>
    // where
    //     P: FnOnce(&TokenKind<'a>) -> bool
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

    fn matches_identifier(&mut self) -> Option<(&'a str, Token<'a>)> {
        if self.check_identifier() {
            let tok = self.advance();
            match tok.kind {
                TokenKind::Identifier(name) => Some((name, tok)),
                _ => unreachable!("Known to be identifier"),
            }
        } else {
            None
        }
    }

    fn consume_identifier(&mut self, message: impl Into<String>) -> Result<(&'a str, Token<'a>), Error> {
        if let Some((name, tok)) = self.matches_identifier() {
            Ok((name, tok))
        } else {
            Err(self.parser_error(message))
        }
    }

    // lookahead 2 tokens

    fn check2(&mut self, token1: TokenKind<'a>, token2: TokenKind<'a>) -> bool {
        let mut dup = self.lexer.clone();
        if let Some(Ok(tok1)) = dup.next() {
            if let Some(Ok(tok2)) = dup.peek() {
                if tok1.kind == token1 && tok2.kind == token2 {
                    return true;
                }
            }
        }
        false
    }

    fn check2_p<P>(&mut self, pred: P) -> bool
    where
        P: FnOnce(&TokenKind<'_>, &TokenKind<'_>) -> bool,
    {
        let mut dup = self.lexer.clone();
        if let Some(Ok(tok1)) = dup.next() {
            if let Some(Ok(tok2)) = dup.peek() {
                if pred(&tok1.kind, &tok2.kind) {
                    return true;
                }
            }
        }
        false
    }
}
