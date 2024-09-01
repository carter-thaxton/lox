use crate::ast::*;
use crate::errors::*;
use colored::Colorize;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use std::time::Instant;

pub enum Callable {
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        line: usize,
        closure: Rc<RefCell<Environment>>,
    },
    Builtin {
        name: String,
        arity: usize,
        fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error<'static>>>,
    },
}

// all this, because we can't derive PartialEq for builtin functions
impl PartialEq for Callable {
    fn eq(&self, other: &Callable) -> bool {
        match (self, other) {
            (
                Callable::Function {
                    name: my_name,
                    params: my_params,
                    body: my_body,
                    line: my_line,
                    ..
                },
                Callable::Function {
                    name,
                    params,
                    body,
                    line,
                    ..
                },
            ) => my_name == name && my_params == params && my_body == body && my_line == line,
            (
                Callable::Builtin {
                    name: my_name,
                    arity: my_arity,
                    ..
                },
                Callable::Builtin { name, arity, .. },
            ) => {
                // match builtins only by name and arity
                my_name == name && my_arity == arity
            }
            _ => false,
        }
    }
}

impl Debug for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Callable::Function { name, params, .. } => {
                write!(f, "function {}(", name)?;
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                    first = false;
                }
                write!(f, ")")?;
            }
            Callable::Builtin { name, arity, .. } => {
                write!(f, "builtin function {}(", name)?;
                for i in 0..*arity {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "_")?;
                }
                write!(f, ")")?;
            }
        }
        todo!()
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Callable::Function { name, .. } => {
                write!(f, "<fn {}>", name)
            }
            Callable::Builtin { .. } => {
                write!(f, "<native fn>")
            }
        }
    }
}

impl Callable {
    pub fn arity(&self) -> usize {
        match self {
            Callable::Function { params, .. } => params.len(),
            Callable::Builtin { arity, .. } => *arity,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
    Callable(Rc<Callable>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Nil => {
                write!(f, "nil")
            }
            Value::True => {
                write!(f, "true")
            }
            Value::False => {
                write!(f, "false")
            }
            Value::Number(n) => {
                // this is arbitrarily different from the way numeric literals are printed
                write!(f, "{}", n)
            }
            Value::String(s) => {
                write!(f, "{}", s)
            }
            Value::Callable(c) => {
                write!(f, "{}", c)
            }
        }
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Nil => Value::Nil,
            Literal::True => Value::True,
            Literal::False => Value::False,
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::False
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::False => false,
            _ => true,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    fn builtin_fn(
        name: impl Into<String>,
        arity: usize,
        fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error<'static>>>,
    ) -> Value {
        Value::Callable(Rc::new(Callable::Builtin {
            name: name.into(),
            arity,
            fcn,
        }))
    }
}

fn compare_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::True, Value::True) => true,
        (Value::False, Value::False) => true,
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        _ => false,
    }
}

fn clock(start_time: &Instant) -> f64 {
    Instant::now().duration_since(*start_time).as_secs_f64()
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    test: bool,
    test_output: VecDeque<String>,
}

impl Interpreter {
    pub fn new(test: bool) -> Self {
        let mut env = Environment::global();

        // built-in globals
        let start_time = Instant::now();
        let clock_fn = Value::builtin_fn(
            "clock",
            0,
            Box::new(move |_| Ok(Value::Number(clock(&start_time)))),
        );

        env.define("clock", clock_fn);

        Interpreter {
            env: Rc::new(RefCell::new(env)),
            test,
            test_output: VecDeque::new(),
        }
    }

    pub fn run(&mut self, program: &Program) -> Result<(), Error<'static>> {
        for stmt in program {
            self.execute(stmt)?;
        }

        if self.test {
            // check that there was no unexpected output
            self.check_final_output()?;
        }

        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value, Error<'static>> {
        match expr {
            Expr::Literal(literal) => Ok(literal.into()),

            Expr::Group(expr) => self.evaluate(expr),

            Expr::Variable(name) => {
                if let Some(val) = self.env.borrow().get(name) {
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

            Expr::Assign { name, right } => {
                let right = self.evaluate(right)?;
                if self.env.borrow_mut().assign(name, right.clone()) {
                    Ok(right)
                } else {
                    Err(Error::runtime_error(format!(
                        "Undefined variable '{}'.",
                        name
                    )))
                }
            }

            Expr::Call {
                callee,
                args,
                line: _,
            } => {
                let callee = self.evaluate(callee)?;
                let args: Result<Vec<Value>, Error<'static>> =
                    args.into_iter().map(|arg| self.evaluate(arg)).collect();
                let args = args?;
                return self.call(callee, &args);
            }

            _ => Err(Error::runtime_error("Unexpected expression.")),
        }
    }

    pub fn call(&mut self, callee: Value, args: &[Value]) -> Result<Value, Error<'static>> {
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
        }
    }

    fn evaluate_to_number(&mut self, expr: &Expr) -> Result<f64, Error<'static>> {
        let val = self.evaluate(expr)?;
        match val {
            Value::Number(n) => Ok(n),
            _ => Err(Error::runtime_error("Operand must be a number.")),
        }
    }

    fn evaluate_to_numbers(
        &mut self,
        left: &Expr,
        right: &Expr,
    ) -> Result<(f64, f64), Error<'static>> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => Ok((left, right)),
            _ => Err(Error::runtime_error("Operands must be numbers.")),
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Error<'static>> {
        match stmt {
            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
            }

            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                self.print(&val);
            }

            Stmt::Var(name, initializer) => {
                if let Some(expr) = initializer {
                    let val = self.evaluate(expr)?;
                    self.env.borrow_mut().define(name, val);
                } else {
                    self.env.borrow_mut().define(name, Value::Nil);
                }
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

            Stmt::IfElse(cond, then_branch, else_branch) => {
                let cond = self.evaluate(cond)?;
                if cond.is_truthy() {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }

            Stmt::While(cond, body) => {
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

            Stmt::Function(name, params, body, line) => {
                let fcn = Value::Callable(Rc::new(Callable::Function {
                    name: name.to_string(),
                    params: params.to_vec(),
                    body: body.to_vec(),
                    line: *line,
                    closure: Rc::clone(&self.env),
                }));

                self.env.borrow_mut().define(name, fcn);
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
    fn check_output(&mut self, expected: &str) -> Result<(), Error<'static>> {
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

    fn check_final_output(&mut self) -> Result<(), Error<'static>> {
        if let Some(actual) = self.test_output.pop_front() {
            return Err(Error::test_output_unexpected(actual));
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Environment {
    variables: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn global() -> Self {
        Environment {
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn new(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            variables: HashMap::new(),
            parent: Some(Rc::clone(&parent)),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.variables.get(name) {
            return Some(val.clone());
        }

        match &self.parent {
            Some(parent) => parent.borrow().get(name),
            None => None,
        }
    }

    // returns true if successful, and false if variable is not defined
    fn assign(&mut self, name: &str, val: Value) -> bool {
        if self.variables.contains_key(name) {
            self.variables.insert(name.to_string(), val);
            return true;
        }

        match &mut self.parent {
            Some(parent) => parent.borrow_mut().assign(name, val),
            None => false,
        }
    }

    fn define(&mut self, name: &str, val: Value) {
        self.variables.insert(name.to_string(), val);
    }

    // fn is_defined(&self, name: &str) -> bool {
    //     if self.variables.contains_key(name) {
    //         return true;
    //     }

    //     match &self.parent {
    //         Some(parent) => parent.is_defined(name),
    //         None => false,
    //     }
    // }
}
