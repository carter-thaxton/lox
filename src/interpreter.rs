use crate::ast::*;
use crate::errors::*;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use colored::Colorize;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
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

pub struct Interpreter {
    env: Environment,
    test: bool,
    test_output: VecDeque<String>,
}

impl Interpreter {
    pub fn new(test: bool) -> Self {
        Interpreter {
            env: Environment::new(),
            test,
            test_output: VecDeque::new(),
        }
    }

    pub fn run<'a>(&mut self, program: &'a Program) -> Result<(), Error<'a>> {
        for stmt in program {
            self.execute(stmt)?;
        }
        Ok(())
    }

    pub fn evaluate<'a>(&mut self, expr: &'a Expr) -> Result<Value, Error<'a>> {
        match expr {
            Expr::Literal(literal) => Ok(literal.into()),

            Expr::Group(expr) => self.evaluate(expr),

            Expr::Variable(name) => {
                if let Some(val) = self.env.get(name) {
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
                if self.env.assign(name, right.clone()) {
                    Ok(right)
                } else {
                    Err(Error::runtime_error(format!(
                        "Undefined variable '{}'.",
                        name
                    )))
                }
            }

            _ => Err(Error::runtime_error("Unexpected expression.")),
        }
    }

    fn evaluate_to_number<'a>(&mut self, expr: &'a Expr) -> Result<f64, Error<'a>> {
        let val = self.evaluate(expr)?;
        match val {
            Value::Number(n) => Ok(n),
            _ => Err(Error::runtime_error("Operand must be a number.")),
        }
    }

    fn evaluate_to_numbers<'a>(
        &mut self,
        left: &'a Expr,
        right: &'a Expr,
    ) -> Result<(f64, f64), Error<'a>> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => Ok((left, right)),
            _ => Err(Error::runtime_error("Operands must be numbers.")),
        }
    }

    fn execute<'a>(&mut self, stmt: &'a Stmt) -> Result<(), Error<'a>> {
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
                    self.env.define(name, val);
                } else {
                    self.env.define(name, Value::Nil);
                }
            }
            Stmt::Block(stmts) => {
                self.env.enter();
                for stmt in stmts {
                    self.execute(stmt)?;
                }
                self.env.exit();
            }
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

    fn check_output<'a>(&mut self, expected: &'a str) -> Result<(), Error<'a>> {
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
}

struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Environment {
    // creates a new environment in the outer-most scope
    fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }

    // enter a new local scope
    fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // exit the outer-most scope
    // panics if attempting to exit the global scope
    fn exit(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Attempt to exit the global scope");
        }
    }

    fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    // returns true if successful, and false if variable is not defined
    fn assign(&mut self, name: &str, val: Value) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return true;
            }
        }
        false
    }

    fn define(&mut self, name: &str, val: Value) {
        for scope in self.scopes.iter_mut().rev() {
            scope.insert(name.to_string(), val);
            return;
        }
    }

    // fn is_defined(&self, name: &str) -> bool {
    //     for scope in self.scopes.iter().rev() {
    //         if scope.contains_key(name) {
    //             return true;
    //         }
    //     }
    //     false
    // }
}
