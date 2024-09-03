use crate::ast::*;
use crate::errors::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

pub enum Callable {
    Function {
        name: Option<String>,
        params: Vec<String>,
        body: Vec<Stmt>,
        line: usize,
        closure: Rc<RefCell<Environment>>,
    },
    Builtin {
        name: String,
        arity: usize,
        fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error>>,
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
                if let Some(name) = name {
                    write!(f, "function {}(", name)?;
                } else {
                    write!(f, "function (")?;
                }
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
            Callable::Function {
                name: Some(name), ..
            } => {
                write!(f, "<fn {}>", name)
            }
            Callable::Function { name: None, .. } => {
                write!(f, "<anonymous fn>")
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

    pub fn builtin_fn(
        name: impl Into<String>,
        arity: usize,
        fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error>>,
    ) -> Value {
        Value::Callable(Rc::new(Callable::Builtin {
            name: name.into(),
            arity,
            fcn,
        }))
    }
}

pub fn compare_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::True, Value::True) => true,
        (Value::False, Value::False) => true,
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        _ => false,
    }
}

#[derive(Clone)]
pub struct Environment {
    values: Vec<Value>,
    by_name: HashMap<String, usize>, // index into values
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn global() -> Self {
        Environment {
            values: vec![],
            by_name: HashMap::new(),
            parent: None,
        }
    }

    pub fn new(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: vec![],
            by_name: HashMap::new(),
            parent: Some(Rc::clone(&parent)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(index) = self.by_name.get(name) {
            return Some(self.values[*index].clone());
        }

        match &self.parent {
            Some(parent) => parent.borrow().get(name),
            None => None,
        }
    }

    pub fn get_at(&self, depth: usize, index: usize) -> Option<Value> {
        if depth == 0 {
            if index < self.values.len() {
                Some(self.values[index].clone())
            } else {
                None
            }
        } else {
            match &self.parent {
                Some(parent) => parent.borrow().get_at(depth-1, index),
                None => None,
            }
        }
    }

    // returns true if successful, and false if variable is not defined
    pub fn assign(&mut self, name: &str, val: Value) -> bool {
        if let Some(index) = self.by_name.get(name) {
            if self.values.len() < index + 1 {
                self.values.resize(index + 1, Value::Nil);
            }
            self.values[*index] = val;
            self.by_name.insert(name.to_string(), *index);
            return true;
        }

        match &mut self.parent {
            Some(parent) => parent.borrow_mut().assign(name, val),
            None => false,
        }
    }

    pub fn assign_at(&mut self, depth: usize, index: usize, val: Value) -> bool {
        if depth == 0 {
            if index < self.values.len() {
                self.values[index] = val;
                true
            } else {
                false
            }
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().assign_at(depth-1, index, val),
                None => false,
            }
        }
    }

    // returns index of variable
    // if variable was already defined, sets the value and returns the existing index
    pub fn define(&mut self, name: &str, val: Value) -> usize {
        if let Some(index) = self.by_name.get(name) {
            self.values[*index] = val;
            *index
        } else {
            let index = self.values.len();
            self.values.push(val);
            self.by_name.insert(name.to_string(), index);
            index
        }
    }
}
