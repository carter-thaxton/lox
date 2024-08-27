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

impl Value {

}


pub fn evaluate(expr: &Expr) -> Result<Value, Error<'_>> {
  match expr {
    Expr::Literal(literal) => Ok(literal.into()),
    _ => Err(Error::runtime_error("Unexpected expression.")),

  }

  // Err(Error::runtime_error("Operands must be numbers."))
  // Ok(Value::Nil)
}
