use crate::ast::*;
use crate::errors::*;
use crate::globals::*;
use crate::runtime::*;
use colored::Colorize;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    test: bool,
    test_output: VecDeque<String>,
}

impl Interpreter {
    pub fn new(test: bool) -> Self {
        let mut env = Environment::global();

        define_globals(&mut env);

        Interpreter {
            env: Rc::new(RefCell::new(env)),
            test,
            test_output: VecDeque::new(),
        }
    }

    pub fn run(&mut self, program: &Program) -> Result<(), Error> {
        for stmt in program {
            self.execute(stmt)?;
        }

        if self.test {
            // check that there was no unexpected output
            self.check_final_output()?;
        }

        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, Error> {
        match expr {
            Expr::Literal(literal) => Ok(literal.into()),

            Expr::Group(expr) => self.evaluate(expr),

            Expr::Variable {
                name,
                depth_and_index,
                ..
            } => {
                if let Some((depth, index)) = depth_and_index {
                    if let Some(val) = self.env.borrow().get_at(*depth, *index) {
                        Ok(val.clone())
                    } else {
                        panic!("Undefined variable at run-time, which was resolved at compile-time: {}", name);
                    }
                } else if let Some(val) = self.env.borrow().get(name) {
                    // may be a global variable, according to lox rules
                    Ok(val.clone())
                } else {
                    Err(Error::runtime_error(format!(
                        "Undefined variable '{}'.",
                        name
                    )))
                }
            }

            Expr::UnaryExpr { op: Op::Not, right } => {
                let right = self.evaluate(right)?.is_truthy();
                Ok((!right).into())
            }

            Expr::UnaryExpr { op: Op::Neg, right } => {
                let right = self.evaluate_to_number(right)?;
                Ok(Value::Number(-right))
            }

            Expr::BinaryExpr {
                op: Op::Mul,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok(Value::Number(left * right))
            }

            Expr::BinaryExpr {
                op: Op::Div,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok(Value::Number(left / right))
            }

            Expr::BinaryExpr {
                op: Op::Add,
                left,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
                    (Value::String(left), Value::String(right)) => {
                        let mut result = left;
                        result.push_str(&right);
                        Ok(Value::String(result))
                    }
                    _ => Err(Error::runtime_error(
                        "Operands must be two numbers or two strings.",
                    )),
                }
            }

            Expr::BinaryExpr {
                op: Op::Sub,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok(Value::Number(left - right))
            }

            Expr::BinaryExpr {
                op: Op::Lt,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left < right).into())
            }

            Expr::BinaryExpr {
                op: Op::Le,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left <= right).into())
            }

            Expr::BinaryExpr {
                op: Op::Gt,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left > right).into())
            }

            Expr::BinaryExpr {
                op: Op::Ge,
                left,
                right,
            } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left >= right).into())
            }

            Expr::BinaryExpr {
                op: Op::Eq,
                left,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                let equal = compare_values(&left, &right);
                Ok(equal.into())
            }

            Expr::BinaryExpr {
                op: Op::Ne,
                left,
                right,
            } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                let equal = compare_values(&left, &right);
                Ok((!equal).into())
            }

            Expr::BinaryExpr {
                op: Op::And,
                left,
                right,
            } => {
                let left = self.evaluate(left)?;
                if left.is_truthy() {
                    // short-circuit and: only evaluate right when left is truthy
                    let right = self.evaluate(right)?;
                    Ok(right)
                } else {
                    Ok(left)
                }
            }

            Expr::BinaryExpr {
                op: Op::Or,
                left,
                right,
            } => {
                let left = self.evaluate(left)?;
                if !left.is_truthy() {
                    // short-circuit or: only evaluate right when left is falsey
                    let right = self.evaluate(right)?;
                    Ok(right)
                } else {
                    Ok(left)
                }
            }

            Expr::Assign {
                name,
                right,
                depth_and_index,
                ..
            } => {
                let right = self.evaluate(right)?;
                if let Some((depth, index)) = depth_and_index {
                    if self
                        .env
                        .borrow_mut()
                        .assign_at(*depth, *index, right.clone())
                    {
                        Ok(right)
                    } else {
                        panic!("Undefined variable at run-time, which was resolved at compile-time: {}", name);
                    }
                } else {
                    Err(Error::runtime_error(format!(
                        "Undefined variable '{}'.",
                        name
                    )))
                }
            }

            Expr::Call { callee, args, .. } => {
                let callee = self.evaluate(callee)?;
                let args: Result<Vec<Value>, Error> =
                    args.into_iter().map(|arg| self.evaluate(arg)).collect();
                let args = args?;
                return self.call(callee, &args);
            }

            Expr::Function { params, body, line } => {
                let fcn = Value::Callable(Rc::new(Callable::Function {
                    name: None,
                    params: params.iter().map(|p| p.0.clone()).collect(),
                    body: body.to_vec(),
                    line: *line,
                    closure: Rc::clone(&self.env),
                }));
                return Ok(fcn);
            }

            _ => Err(Error::runtime_error("Unexpected expression.")),
        }
    }

    pub fn call(&mut self, callee: Value, args: &[Value]) -> Result<Value, Error> {
        let callee = match callee {
            Value::Callable(callee) => callee,
            _ => return Err(Error::runtime_error("Can only call functions and classes.")),
        };

        if args.len() != callee.arity() {
            return Err(Error::runtime_error(format!(
                "Expected {} arguments but got {}.",
                callee.arity(),
                args.len()
            )));
        }

        match &*callee {
            Callable::Function {
                params,
                body,
                closure,
                ..
            } => {
                let orig_env = self.enter(closure.clone());

                for (i, param) in params.iter().enumerate() {
                    self.env.borrow_mut().define(param, args[i].clone());
                }

                for stmt in body {
                    let result = self.execute(&stmt);

                    match result {
                        // handle return value
                        Err(Error {
                            kind: ErrorKind::ReturnValue(value),
                            ..
                        }) => {
                            self.env = orig_env;
                            return Ok(value);
                        }
                        Err(err) => {
                            self.env = orig_env;
                            return Err(err);
                        }
                        _ => {}
                    }
                }

                self.env = orig_env;
                Ok(Value::Nil)
            }

            Callable::Builtin { fcn, .. } => Ok(fcn(args)?),

            Callable::Class { .. } => Err(Error::runtime_error("Classes not yet supported")),
        }
    }

    fn evaluate_to_number(&mut self, expr: &Expr) -> Result<f64, Error> {
        let val = self.evaluate(expr)?;
        match val {
            Value::Number(n) => Ok(n),
            _ => Err(Error::runtime_error("Operand must be a number.")),
        }
    }

    fn evaluate_to_numbers(&mut self, left: &Expr, right: &Expr) -> Result<(f64, f64), Error> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => Ok((left, right)),
            _ => Err(Error::runtime_error("Operands must be numbers.")),
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
            }

            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                self.print(&val);
            }

            Stmt::Var { name, init, .. } => {
                let val = if let Some(expr) = init {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };
                self.env.borrow_mut().define(name, val);
            }

            Stmt::Block(stmts) => {
                let orig_env = self.enter(self.env.clone());
                for stmt in stmts {
                    let result = self.execute(stmt);
                    if result.is_err() {
                        self.env = orig_env;
                        return result;
                    }
                }
                self.env = orig_env;
            }

            Stmt::IfElse {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond = self.evaluate(cond)?;
                if cond.is_truthy() {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }

            Stmt::While { cond, body } => {
                while self.evaluate(cond)?.is_truthy() {
                    let result = self.execute(&body);

                    // handle break and continue statements, implemented as special errors
                    match result {
                        Err(Error {
                            kind: ErrorKind::BreakLoop,
                            ..
                        }) => {
                            return Ok(());
                        }
                        Err(Error {
                            kind: ErrorKind::ContinueLoop,
                            ..
                        }) => {
                            continue;
                        }
                        _ => {}
                    }
                    result?;
                }
            }

            Stmt::Function {
                name,
                params,
                body,
                line,
            } => {
                let fcn = Value::Callable(Rc::new(Callable::Function {
                    name: Some(name.to_string()),
                    params: params.iter().map(|p| p.0.clone()).collect(),
                    body: body.to_vec(),
                    line: *line,
                    closure: Rc::clone(&self.env),
                }));

                self.env.borrow_mut().define(name, fcn);
            }

            Stmt::Class {
                name,
                methods,
                line,
            } => {
                let class = Value::Callable(Rc::new(Callable::Class {
                    name: name.to_string(),
                    methods: methods.to_vec(),
                    line: *line,
                }));

                self.env.borrow_mut().define(name, class);
            }

            Stmt::Return(expr) => {
                let val = if let Some(expr) = expr {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };

                return Err(Error::return_value(val));
            }

            Stmt::Break => {
                return Err(Error::break_loop());
            }

            Stmt::Continue => {
                return Err(Error::continue_loop());
            }

            // == TEST ==
            Stmt::ExpectOutput(txt) => {
                if self.test {
                    self.check_output(txt)?;
                }
            }

            Stmt::ExpectRuntimeError(msg) => {
                if self.test {
                    // reaching here means we did NOT actually get a runtime error before this
                    return Err(Error::test_expected_runtime_error(msg));
                }
            }
        }
        Ok(())
    }

    fn enter(&mut self, env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let orig_env = Rc::clone(&self.env);
        self.env = Rc::new(RefCell::new(Environment::new(env)));
        orig_env
    }

    fn print(&mut self, val: &Value) {
        if self.test {
            // keep track of each output line
            for line in format!("{}", val).lines() {
                self.test_output.push_back(line.to_string());
            }
        } else {
            // simply print to stdout
            println!("{}", val);
        }
    }

    // == TEST ==
    fn check_output(&mut self, expected: &str) -> Result<(), Error> {
        if let Some(actual) = self.test_output.pop_front() {
            if actual == expected {
                println!("{}: expect: {}", "PASS".green(), actual);
                return Ok(());
            } else {
                return Err(Error::test_output_mismatch(expected, actual));
            }
        } else {
            return Err(Error::test_output_missing(expected));
        }
    }

    fn check_final_output(&mut self) -> Result<(), Error> {
        if let Some(actual) = self.test_output.pop_front() {
            return Err(Error::test_output_unexpected(actual));
        }
        Ok(())
    }
}
