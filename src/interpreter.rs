use crate::ast::*;
use crate::errors::*;
use std::fmt::{Display, Formatter};

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

pub fn evaluate(expr: &Expr) -> Result<Value, Error<'_>> {
    match expr {
        Expr::Literal(literal) => Ok(literal.into()),
        Expr::Group(expr) => evaluate(expr),

        Expr::UnaryExpr { op: Op::Not, right } => {
            let right = evaluate(right)?.is_truthy();
            Ok((!right).into())
        }

        Expr::UnaryExpr { op: Op::Neg, right } => {
            let right = evaluate_to_number(right)?;
            Ok(Value::Number(-right))
        }

        Expr::BinaryExpr { op: Op::Mul, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok(Value::Number(left * right))
        }

        Expr::BinaryExpr { op: Op::Div, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok(Value::Number(left / right))
        }

        Expr::BinaryExpr { op: Op::Add, left, right } => {
            let left = evaluate(left)?;
            let right = evaluate(right)?;
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
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok(Value::Number(left - right))
        }

        Expr::BinaryExpr { op: Op::Lt, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok((left < right).into())
        }

        Expr::BinaryExpr { op: Op::Le, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok((left <= right).into())
        }

        Expr::BinaryExpr { op: Op::Gt, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok((left > right).into())
        }

        Expr::BinaryExpr { op: Op::Ge, left, right } => {
            let (left, right) = evaluate_to_numbers(left, right)?;
            Ok((left >= right).into())
        }

        _ => Err(Error::runtime_error("Unexpected expression.")),
    }
}

fn evaluate_to_number(expr: &Expr) -> Result<f64, Error<'_>> {
    let val = evaluate(expr)?;
    match val {
        Value::Number(n) => {
            Ok(n)
        }
        _ => Err(Error::runtime_error("Operand must be a number."))
    }
}

fn evaluate_to_numbers<'a>(left: &'a Expr, right: &'a Expr) -> Result<(f64, f64), Error<'a>> {
    let left = evaluate(left)?;
    let right = evaluate(right)?;
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => {
            Ok((left, right))
        }
        _ => Err(Error::runtime_error("Operands must be numbers."))
    }
}
