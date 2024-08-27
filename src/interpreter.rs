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
            let val = evaluate(right)?;
            Ok((!val.is_truthy()).into())
        }

        Expr::UnaryExpr { op: Op::Neg, right } => {
            let val = evaluate_to_number(right)?;
            Ok(Value::Number(-val))
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
