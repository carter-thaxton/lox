use crate::ast::*;
use crate::errors::*;
use crate::globals::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

pub struct FunctionDecl {
    pub name: Option<String>,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub line: usize,
    pub is_init: bool,
}

impl Display for FunctionDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if let Some(name) = &self.name {
            write!(f, "<fn {}>", name)
        } else {
            write!(f, "<anonymous fn>")
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub declaration: Rc<FunctionDecl>,
    pub closure: Rc<RefCell<Environment>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        // use reference equality
        Rc::ptr_eq(&self.declaration, &other.declaration) && Rc::ptr_eq(&self.closure, &other.closure)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.declaration)
    }
}

impl Function {
    pub fn new(name: Option<String>, params: &[(String, usize)], body: &[Stmt], line: usize, is_init: bool, env: &Rc<RefCell<Environment>>) -> Self {
        let declaration = FunctionDecl {
            name,
            params: params.iter().map(|p| p.0.clone()).collect(),
            body: body.to_vec(),
            line,
            is_init,
        };

        Function {
            declaration: Rc::new(declaration),
            closure: Rc::clone(env),
        }
    }

    pub fn bind(&self, instance: Instance) -> Function {
        let closure = Environment::new(Rc::clone(&self.closure));
        closure.borrow_mut().define("this", Value::Instance(instance)); // always at index 0 in new environment
        Function {
            declaration: self.declaration.clone(),
            closure,
        }
    }

    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}

pub struct BuiltinFunction {
    name: String,
    arity: usize,
    fcn: Box<dyn Fn(&[Value]) -> Result<Value, Error>>,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &BuiltinFunction) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<native fn>")
    }
}

impl BuiltinFunction {
    pub fn call(&self, args: &[Value]) -> Result<Value, Error> {
        if args.len() != self.arity {
            panic!(
                "Call to builtin function '{}' expected {} arguments but got {}.",
                self.name,
                self.arity,
                args.len()
            );
        }
        (self.fcn)(args)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, Function>,
    line: usize,
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

impl Class {
    pub fn new(name: impl Into<String>, superclass: Option<Rc<Class>>, methods: &[Stmt], line: usize, env: &Rc<RefCell<Environment>>) -> Self {
        let mut methods_map: HashMap<String, Function> = HashMap::new();
        for method in methods {
            match method {
                Stmt::Function { name, params, body, line } => {
                    let is_init = name == "init"; // detect specially-named 'init' method
                    let fcn = Function::new(Some(name.to_string()), &params, &body, *line, is_init, env);
                    methods_map.insert(name.to_string(), fcn);
                }
                _ => {
                    panic!("Stmt::Class methods may only contain Stmt::Functions");
                }
            }
        }

        Class {
            name: name.into(),
            superclass,
            methods: methods_map,
            line,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&Function> {
        if let Some(method) = self.methods.get(name) {
            Some(method)
        } else if let Some(superclass) = &self.superclass {
            superclass.find_method(name)
        } else {
            None
        }
    }

    pub fn init_arity(&self) -> usize {
        if let Some(init) = self.find_method("init") {
            init.arity()
        } else {
            0
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Callable {
    Function(Function),
    Builtin(Rc<BuiltinFunction>),
    Class(Rc<Class>),
}

impl Debug for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Callable::Function(fcn) => {
                write!(f, "{}", fcn.declaration)
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
            Callable::Function(f) => f.arity(),
            Callable::Builtin(f) => f.arity,
            Callable::Class(c) => c.init_arity(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    class: Rc<Class>,
    fields: Rc<RefCell<HashMap<String, Value>>>,
}

impl PartialEq for Instance {
    fn eq(&self, other: &Instance) -> bool {
        // use reference equality
        Rc::ptr_eq(&self.class, &other.class) && Rc::ptr_eq(&self.fields, &other.fields)
    }
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
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.fields.borrow().get(name) {
            Some(val.clone())
        } else if let Some(method) = self.class.find_method(name) {
            let method = method.bind(self.clone());
            Some(Value::Callable(Callable::Function(method)))
        } else {
            None
        }
    }

    pub fn set(&self, name: impl Into<String>, value: Value) {
        self.fields.borrow_mut().insert(name.into(), value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
    Callable(Callable),
    Instance(Instance),
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
                write!(f, "{}", i)
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
        Value::Callable(Callable::Builtin(Rc::new(fcn)))
    }
}

#[derive(Clone)]
pub struct Environment {
    values: Vec<Value>,
    by_name: HashMap<String, usize>, // index into values
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn global() -> Rc<RefCell<Environment>> {
        let mut env = Environment {
            values: vec![],
            by_name: HashMap::new(),
            parent: None,
        };

        define_globals(&mut env);

        Rc::new(RefCell::new(env))
    }

    pub fn new(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            values: vec![],
            by_name: HashMap::new(),
            parent: Some(Rc::clone(&parent)),
        }))
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
