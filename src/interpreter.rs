use crate::ast::*;
use crate::errors::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

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

pub fn evaluate<'a>(expr: &'a Expr, env: &mut Environment) -> Result<Value, Error<'a>> {
    match expr {
        Expr::Literal(literal) => Ok(literal.into()),

        Expr::Group(expr) => evaluate(expr, env),

        Expr::Variable(name) => {
            if let Some(val) = env.get(name) {
                Ok(val.clone())
            } else {
                Err(Error::runtime_error(format!("Undefined variable '{}'.", name)))
            }
        }

        Expr::UnaryExpr { op: Op::Not, right } => {
            let right = evaluate(right, env)?.is_truthy();
            Ok((!right).into())
        }

        Expr::UnaryExpr { op: Op::Neg, right } => {
            let right = evaluate_to_number(right, env)?;
            Ok(Value::Number(-right))
        }

        Expr::BinaryExpr { op: Op::Mul, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok(Value::Number(left * right))
        }

        Expr::BinaryExpr { op: Op::Div, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok(Value::Number(left / right))
        }

        Expr::BinaryExpr { op: Op::Add, left, right } => {
            let left = evaluate(left, env)?;
            let right = evaluate(right, env)?;
            match (left, right) {
                (Value::Number(left), Value::Number(right)) => {
                    Ok(Value::Number(left + right))
                }
                (Value::String(left), Value::String(right)) => {
                    let mut result = left;
                    result.push_str(&right);
                    Ok(Value::String(result))
                }
                _ => Err(Error::runtime_error("Operands must be two numbers or two strings."))
            }
        }

        Expr::BinaryExpr { op: Op::Sub, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok(Value::Number(left - right))
        }

        Expr::BinaryExpr { op: Op::Lt, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok((left < right).into())
        }

        Expr::BinaryExpr { op: Op::Le, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok((left <= right).into())
        }

        Expr::BinaryExpr { op: Op::Gt, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok((left > right).into())
        }

        Expr::BinaryExpr { op: Op::Ge, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right, env)?;
            Ok((left >= right).into())
        }

        Expr::BinaryExpr { op: Op::Eq, left, right } => {
            let left = evaluate(left, env)?;
            let right = evaluate(right, env)?;

            let equal = compare_values(&left, &right);
            Ok(equal.into())
        }

        Expr::BinaryExpr { op: Op::Ne, left, right } => {
            let left = evaluate(left, env)?;
            let right = evaluate(right, env)?;

            let equal = compare_values(&left, &right);
            Ok((!equal).into())
        }

        Expr::Assign { name, right } => {
            let right = evaluate(right, env)?;
            if !env.is_defined(name) {
                return Err(Error::runtime_error(format!("Undefined variable '{}'.", name)))
            }
            env.set(name, right.clone());
            Ok(right)
        }

        _ => Err(Error::runtime_error("Unexpected expression.")),
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

fn evaluate_to_number<'a>(expr: &'a Expr, env: &mut Environment) -> Result<f64, Error<'a>> {
    let val = evaluate(expr, env)?;
    match val {
        Value::Number(n) => {
            Ok(n)
        }
        _ => Err(Error::runtime_error("Operand must be a number."))
    }
}

fn evaluate_to_numbers<'a>(left: &'a Expr, right: &'a Expr, env: &mut Environment) -> Result<(f64, f64), Error<'a>> {
    let left = evaluate(left, env)?;
    let right = evaluate(right, env)?;
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => {
            Ok((left, right))
        }
        _ => Err(Error::runtime_error("Operands must be numbers."))
    }
}


pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new()
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }

    pub fn set(&mut self, name: &str, val: Value) {
        self.values.insert(name.to_string(), val);
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }
}

pub fn run(program: &Program) -> Result<(), Error<'_>> {
    let mut env = Environment::new();

    for stmt in program {
        execute(stmt, &mut env)?;
    }
    Ok(())
}

fn execute<'a>(stmt: &'a Stmt, env: &mut Environment) -> Result<(), Error<'a>> {
    match stmt {
        Stmt::Expr(expr) => {
            evaluate(expr, env)?;
        }
        Stmt::Print(expr) => {
            let val = evaluate(expr, env)?;
            println!("{}", val);
        }
        Stmt::Var(name, initializer) => {
            if let Some(expr) = initializer {
                let val = evaluate(expr, env)?;
                env.set(name, val);
            } else {
                env.set(name, Value::Nil);
            }
        }
        Stmt::Block(stmts) => {
            for stmt in stmts {
                execute(stmt, env)?;
            }
        }
    }
    Ok(())
}
