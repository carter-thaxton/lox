use crate::ast::*;
use crate::errors::*;
use crate::globals::*;
use std::collections::HashMap;

struct Scopes<'a>(Vec<HashMap<&'a str, (usize, bool)>>);

impl<'a> Scopes<'a> {
    fn new() -> Self {
        Scopes(vec![HashMap::new()])
    }

    fn push(&mut self) {
        self.0.push(HashMap::new());
    }

    fn pop(&mut self) {
        if self.depth() == 0 {
            panic!("Attempt to pop from global scope in resolver");
        }
        self.0.pop();
    }

    fn depth(&self) -> usize {
        self.0.len() - 1
    }

    fn current_scope(&self) -> &HashMap<&'a str, (usize, bool)> {
        self.0.last().expect("Always have at least one scope")
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<&'a str, (usize, bool)> {
        self.0.last_mut().expect("Always have at least one scope")
    }

    // declares a variable in the current scope
    // returns false if variable was already declared in this scope, and true otherwise
    fn declare(&mut self, name: &'a str) -> bool {
        let s = self.current_scope_mut();
        if s.contains_key(name) {
            false
        } else {
            let index = s.len();
            s.insert(name, (index, false));
            true
        }
    }

    // defines a variable in the current scope, panics if it was not already declared
    fn define(&mut self, name: &'a str) {
        let s = self.current_scope_mut();
        if let Some((_index, defined)) = s.get_mut(name) {
            *defined = true;
        } else {
            panic!("define({}) called without corresponding declare()", name);
        }
    }

    fn depth_and_index_of(&self, name: &'a str) -> Option<(usize, usize)> {
        for (depth, scope) in self.0.iter().rev().enumerate() {
            if let Some((index, _defined)) = scope.get(name) {
                return Some((depth, *index));
            }
        }
        None
    }

    fn is_global(&self, name: &'a str) -> bool {
        if let Some((depth, _index)) = self.depth_and_index_of(name) {
            depth == self.depth()
        } else {
            // not found - may in fact be global in lox
            true
        }
    }

    // like declare, but raises an appropriate error when there's a duplicate name in the same scope
    fn declare_and_check<'b: 'a>(&mut self, name: &'b str, line: usize) -> Result<(), Error> {
        if !self.declare(name) && !self.is_global(name) {
            return Err(Error::parser_error_on_line_at_token(
                line,
                name.to_string(),
                "Already a variable with this name in this scope.",
            ));
        }
        Ok(())
    }
}

//
// second pass - resolve declarations
//
// checks variables, assignments, and functions, producing appropriate errors.
//
// currently, the only mutation to the AST is to set the 'depth_and_index' on any Variable and Assign expressions.
//
pub fn resolve(program: &mut Program) -> Result<(), Error> {
    let mut scopes = Scopes::new();

    for name in global_names() {
        scopes.declare(name);
    }

    for stmt in program {
        resolve_stmt(stmt, &mut scopes)?;
    }

    Ok(())
}

fn resolve_stmt<'a>(stmt: &'a mut Stmt, scopes: &mut Scopes<'a>) -> Result<(), Error> {
    match stmt {
        // these are the juicy cases
        Stmt::Var { name, init, line } => {
            scopes.declare_and_check(name, *line)?;
            if let Some(init) = init {
                resolve_expr(init, scopes)?;
            }
            scopes.define(name);
        }
        Stmt::Function {
            name,
            params,
            body,
            line,
        } => {
            scopes.declare_and_check(name, *line)?;
            scopes.define(name);
            resolve_function(params, body, scopes)?;
        }
        Stmt::Class {
            name,
            methods: _,
            line,
        } => {
            scopes.declare_and_check(name, *line)?;
            // TODO: resolve methods
        }

        // below simply walks the AST
        Stmt::Expr(expr) => {
            resolve_expr(expr, scopes)?;
        }
        Stmt::Print(expr) => {
            resolve_expr(expr, scopes)?;
        }
        Stmt::Block(stmts) => {
            // defer?
            scopes.push();
            for stmt in stmts {
                match resolve_stmt(stmt, scopes) {
                    Err(err) => {
                        scopes.pop();
                        return Err(err);
                    }
                    _ => {}
                }
            }
            scopes.pop();
        }
        Stmt::IfElse {
            cond,
            then_branch,
            else_branch,
        } => {
            resolve_expr(cond, scopes)?;
            resolve_stmt(then_branch, scopes)?;
            if let Some(else_branch) = else_branch {
                resolve_stmt(else_branch, scopes)?;
            }
        }
        Stmt::While { cond, body } => {
            resolve_expr(cond, scopes)?;
            resolve_stmt(body, scopes)?;
        }
        Stmt::Return(expr) => {
            if let Some(expr) = expr {
                resolve_expr(expr, scopes)?;
            }
        }

        _ => {}
    }
    Ok(())
}

fn resolve_expr<'a>(expr: &'a mut Expr, scopes: &mut Scopes<'a>) -> Result<(), Error> {
    match expr {
        // these are the juicy cases
        Expr::Variable {
            name,
            line,
            depth_and_index,
        } => {
            let name: &str = name;
            if matches!(scopes.current_scope().get(name), Some((_, false)))
                && !scopes.is_global(name)
            {
                return Err(Error::parser_error_on_line_at_token(
                    *line,
                    name.to_string(),
                    "Can't read local variable in its own initializer.",
                ));
            }

            if let Some((depth, index)) = scopes.depth_and_index_of(name) {
                *depth_and_index = Some((depth, index));
            } else {
                // TODO: handle references to undefined variables at compile-time?
            }
        }
        Expr::Assign {
            name,
            right,
            depth_and_index,
            ..
        } => {
            resolve_expr(right, scopes)?;

            let name: &str = name;
            if let Some((depth, index)) = scopes.depth_and_index_of(name) {
                *depth_and_index = Some((depth, index));
            } else {
                // TODO: handle references to undefined variables at compile-time?
            }
        }
        Expr::Function { params, body, .. } => {
            resolve_function(params, body, scopes)?;
        }

        // below simply walks the AST
        Expr::Literal(_) => {}
        Expr::UnaryExpr { right, .. } => {
            resolve_expr(right, scopes)?;
        }
        Expr::BinaryExpr { left, right, .. } => {
            resolve_expr(left, scopes)?;
            resolve_expr(right, scopes)?;
        }
        Expr::Group(expr) => {
            resolve_expr(expr, scopes)?;
        }
        Expr::Call { callee, args, .. } => {
            resolve_expr(callee, scopes)?;
            for arg in args {
                resolve_expr(arg, scopes)?;
            }
        }
    }

    Ok(())
}

fn resolve_function<'a>(
    params: &'a [(String, usize)],
    body: &'a mut [Stmt],
    scopes: &mut Scopes<'a>,
) -> Result<(), Error> {
    // be sure to pop this before exiting
    scopes.push();
    // defer! {
    //     scopes.pop();
    // }

    for (param, line) in params {
        if !scopes.declare(param) {
            return Err(Error::parser_error_on_line_at_token(
                *line,
                param,
                "Already a variable with this name in this scope.",
            ));
        }
        scopes.define(param);
    }

    for stmt in body {
        // better way to do a 'finally' or 'defer' in rust?
        match resolve_stmt(stmt, scopes) {
            Err(err) => {
                scopes.pop();
                return Err(err);
            }
            _ => {}
        }
    }

    scopes.pop();

    Ok(())
}
