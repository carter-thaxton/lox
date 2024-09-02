use crate::ast::*;
use crate::errors::*;
use std::collections::HashMap;

struct Scopes<'a>(Vec<HashMap<&'a str, bool>>);

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

    fn current_scope(&self) -> &HashMap<&'a str, bool> {
        self.0.last().expect("Always have at least one scope")
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<&'a str, bool> {
        self.0.last_mut().expect("Always have at least one scope")
    }

    // declares a variable in the current scope
    // returns false if variable was already declared in this scope, and true otherwise
    fn declare(&mut self, name: &'a str) -> bool {
        let declared = self.current_scope_mut().insert(name, false).is_some();
        !declared
    }

    // defines a variable in the current scope, panics if it was not already declared
    fn define(&mut self, name: &'a str) {
        let declared = self.current_scope_mut().insert(name, true).is_some();
        if !declared {
            panic!("define({}) called without corresponding declare()", name);
        }
    }

    fn depth_of(&self, name: &'a str) -> Option<usize> {
        for (depth, scope) in self.0.iter().rev().enumerate() {
            if scope.contains_key(name) {
                return Some(depth);
            }
        }
        None
    }
}


//
// second pass - resolve declarations
//
// checks variables, assignments, and functions, producing appropriate errors.
//
// currently, the only mutation to the AST is to set the 'depth' on any Variable and Assign expressions.
// TODO: do anonymous function expressions need any special treatment?
//
pub fn resolve(program: &mut Program) -> Result<(), Error> {
    let mut scopes = Scopes::new();

    for stmt in program {
        resolve_stmt(stmt, &mut scopes)?;
    }

    Ok(())
}

fn resolve_stmt<'a>(
    stmt: &'a mut Stmt,
    scopes: &mut Scopes<'a>,
) -> Result<(), Error> {
    match stmt {
        // these are the juicy cases
        Stmt::Function {
            name, params, body, line
        } => {
            if !scopes.declare(name) {
                return Err(Error::parser_error_on_line(*line, "Already a variable with this name in this scope."));
            }
            scopes.define(name);
            resolve_function(params, body, scopes)?;
        }
        Stmt::Var { name, init, line } => {
            if !scopes.declare(name) {
                return Err(Error::parser_error_on_line(*line, "Already a variable with this name in this scope."));
            }
            if let Some(init) = init {
                resolve_expr(init, scopes)?;
            }
            scopes.define(name);
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

fn resolve_expr<'a>(
    expr: &'a mut Expr,
    scopes: &mut Scopes<'a>,
) -> Result<(), Error> {
    match expr {
        // these are the juicy cases
        Expr::Variable { name, line, depth } => {
            let name: &str = name;
            if scopes.current_scope().get(name) == Some(&false) {
                return Err(Error::parser_error_on_line(*line, "Can't read local variable in its own initializer."));
            }

            if let Some(d) = scopes.depth_of(name) {
                // resolved variable
                *depth = Some(d);
            } else {
                // TODO: handle references to undefined variables
            }
        }
        Expr::Assign { name, right, depth, .. } => {
            resolve_expr(right, scopes)?;

            let name: &str = name;
            if let Some(d) = scopes.depth_of(name) {
                // resolved variable
                *depth = Some(d);
            } else {
                // TODO: handle references to undefined variables
            }
        }
        Expr::Function { .. } => {
            todo!("Handle anonymous function expressions")
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
    params: &'a [String],
    body: &'a mut [Stmt],
    scopes: &mut Scopes<'a>,
) -> Result<(), Error> {

    // be sure to pop this before exiting
    scopes.push();
    // defer! {
    //     scopes.pop();
    // }

    for param in params {
        if !scopes.declare(param) {
            todo!("Return error on duplicate parameter name");
        }
        scopes.define(param);
    }

    for stmt in body {
        // better way to do a 'finally' or 'defer' in rust?
        match resolve_stmt(stmt, scopes) {
            Err(err) => {
                scopes.pop();
                return Err(err)
            }
            _ => {}
        }
    }

    scopes.pop();

    Ok(())
}
