use crate::ast::*;
use crate::errors::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub line: usize,
    pub closure: Rc<RefCell<Environment>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        self.name == other.name && self.params == other.params && self.body == other.body && self.line == other.line
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(name) = &self.name {
            write!(f, "function {}(", name)?;
        } else {
            write!(f, "function (")?;
        }
        let mut first = true;
        for param in &self.params {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
            first = false;
        }
        write!(f, ")")
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(name) = &self.name {
            write!(f, "<fn {}>", name)
        } else {
            write!(f, "<anonymous fn>")
        }
    }
}

pub struct BuiltinFunction {
    pub name: String,
    pub arity: usize,
    pub fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error>>,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &BuiltinFunction) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "builtin function {}(", self.name)?;
        for i in 0..self.arity {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "_")?;
        }
        write!(f, ")")
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<native fn>")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: Vec<Stmt>,
    pub line: usize,
    // closure?
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub enum Callable {
    Function(Function),
    Builtin(BuiltinFunction),
    Class(Rc<Class>),
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Callable::Function(fcn) => {
                write!(f, "{}", fcn)
            }
            Callable::Builtin(fcn) => {
                write!(f, "{}", fcn)
            }
            Callable::Class(class) => {
                write!(f, "{}", class)
            }
        }
    }
}

impl Callable {
    pub fn arity(&self) -> usize {
        match self {
            Callable::Function(f) => f.params.len(),
            Callable::Builtin(f) => f.arity,
            Callable::Class(_c) => 0, // TODO: support constructors
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} instance", self.class.name)
    }
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Self {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.fields.get(name)
    }

    pub fn set(&mut self, name: impl Into<String>, value: Value) {
        self.fields.insert(name.into(), value);
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
    Instance(Rc<RefCell<Instance>>),
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
            Value::Instance(i) => {
                write!(f, "{}", i.borrow())
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

    pub fn builtin_fn(name: impl Into<String>, arity: usize, fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error>>) -> Value {
        let fcn = BuiltinFunction {
            name: name.into(),
            arity,
            fcn,
        };
        Value::Callable(Rc::new(Callable::Builtin(fcn)))
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
                Some(parent) => parent.borrow().get_at(depth - 1, index),
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
                Some(parent) => parent.borrow_mut().assign_at(depth - 1, index, val),
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
