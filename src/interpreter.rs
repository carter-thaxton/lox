use crate::ast::*;
use crate::errors::*;
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
        let env = Environment::global();

        // wrap globals in a new scope, to match depth/index from resolver, which doesn't know about built-in globals
        let env = Environment::new(env);

        Interpreter {
            env,
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

            Expr::Variable { name, depth_and_index, .. } => {
                if let Some((depth, index)) = depth_and_index {
                    let Some(val) = self.env.borrow().get_at(*depth, *index) else {
                        panic!("Undefined variable at run-time, which was resolved at compile-time: {}", name);
                    };
                    Ok(val.clone())
                } else if let Some(val) = self.env.borrow().get(name) {
                    // may be a global variable, according to lox rules
                    Ok(val.clone())
                } else {
                    Err(Error::runtime_error(format!("Undefined variable '{}'.", name)))
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

            Expr::BinaryExpr { op: Op::Mul, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok(Value::Number(left * right))
            }

            Expr::BinaryExpr { op: Op::Div, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok(Value::Number(left / right))
            }

            Expr::BinaryExpr { op: Op::Add, left, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
                    (Value::String(left), Value::String(right)) => {
                        let mut result = left;
                        result.push_str(&right);
                        Ok(Value::String(result))
                    }
                    _ => Err(Error::runtime_error("Operands must be two numbers or two strings.")),
                }
            }

            Expr::BinaryExpr { op: Op::Sub, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok(Value::Number(left - right))
            }

            Expr::BinaryExpr { op: Op::Lt, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left < right).into())
            }

            Expr::BinaryExpr { op: Op::Le, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left <= right).into())
            }

            Expr::BinaryExpr { op: Op::Gt, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left > right).into())
            }

            Expr::BinaryExpr { op: Op::Ge, left, right } => {
                let (left, right) = self.evaluate_to_numbers(left, right)?;
                Ok((left >= right).into())
            }

            Expr::BinaryExpr { op: Op::Eq, left, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                let equal = left == right;
                Ok(equal.into())
            }

            Expr::BinaryExpr { op: Op::Ne, left, right } => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                let equal = left == right;
                Ok((!equal).into())
            }

            Expr::BinaryExpr { op: Op::And, left, right } => {
                let left = self.evaluate(left)?;
                if left.is_truthy() {
                    // short-circuit and: only evaluate right when left is truthy
                    let right = self.evaluate(right)?;
                    Ok(right)
                } else {
                    Ok(left)
                }
            }

            Expr::BinaryExpr { op: Op::Or, left, right } => {
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
                    if self.env.borrow_mut().assign_at(*depth, *index, right.clone()) {
                        Ok(right)
                    } else {
                        panic!("Undefined variable at run-time, which was resolved at compile-time: {}", name);
                    }
                } else {
                    Err(Error::runtime_error(format!("Undefined variable '{}'.", name)))
                }
            }

            Expr::Call { callee, args, .. } => {
                let callee = self.evaluate(callee)?;
                let args: Result<Vec<Value>, Error> = args.into_iter().map(|arg| self.evaluate(arg)).collect();
                let args = args?;
                self.call(callee, &args)
            }

            Expr::Function { params, body, line } => {
                let fcn = Function::new(None, params, body, *line, false, &self.env);
                let fcn_val = Value::Callable(Callable::Function(fcn));
                Ok(fcn_val)
            }

            Expr::Get { object, property, .. } => {
                let object = self.evaluate(object)?;
                match object {
                    Value::Instance(instance) => {
                        if let Some(value) = instance.get(property) {
                            Ok(value.clone())
                        } else {
                            Err(Error::runtime_error(format!("Undefined property '{}'.", property)))
                        }
                    }
                    _ => Err(Error::runtime_error("Only instances have properties.")),
                }
            }

            Expr::Set { object, property, value, .. } => {
                let object = self.evaluate(object)?;
                let value = self.evaluate(value)?;
                match object {
                    Value::Instance(instance) => {
                        instance.set(property, value.clone());
                        Ok(value)
                    }
                    _ => Err(Error::runtime_error("Only instances have fields.")),
                }
            }

            Expr::This { depth_and_index, .. } => {
                let Some((depth, index)) = depth_and_index else {
                    panic!("Failed to resolve 'this' at compile-time.");
                };

                assert_eq!(*index, 0, "'this' should always have index 0.");

                let Some(val) = self.env.borrow().get_at(*depth, *index) else {
                    panic!("Undefined 'this' at run-time, which was resolved at compile-time.");
                };

                Ok(val.clone())
            }

            Expr::Super { method, depth_and_index, .. } => {
                let Some((depth, index)) = depth_and_index else {
                    panic!("Failed to resolve 'super' at compile-time.");
                };

                assert_eq!(*index, 0, "'super' should always have index 0.");

                let Some(superclass_val) = self.env.borrow().get_at(*depth, *index) else {
                    panic!("Undefined 'super' at run-time, which was resolved at compile-time.");
                };

                let superclass = match superclass_val {
                    Value::Callable(Callable::Class(superclass)) => superclass,
                    _ => panic!("'super' should always resolve to a class."),
                };

                if let Some(method) = superclass.find_method(method) {
                    assert!(*depth > 0, "Depth of 'super' should always be greater than 0.");

                    let this_val = self
                        .env
                        .borrow()
                        .get_at(depth - 1, 0)
                        .expect("Superclass should always define 'this' in depth before 'super'");

                    let this = match this_val {
                        Value::Instance(this) => this,
                        _ => panic!("'this' should always be an instance."),
                    };

                    let result = method.bind(this);
                    Ok(Value::Callable(Callable::Function(result)))
                } else {
                    Err(Error::runtime_error(format!("Undefined property '{}'.", method)))
                }
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

        match callee {
            Callable::Function(f) => {
                let orig_env = self.enter(Rc::clone(&f.closure));

                for (i, param) in f.declaration.params.iter().enumerate() {
                    self.env.borrow_mut().define(param, args[i].clone());
                }

                for stmt in &f.declaration.body {
                    let result = self.execute(&stmt);

                    match result {
                        // handle return value
                        Err(Error {
                            kind: ErrorKind::ReturnValue(value),
                            ..
                        }) => {
                            if f.declaration.is_init {
                                if value != Value::Nil {
                                    panic!("Can't return a value from an initializer.  This should be checked at compile-time.");
                                }
                                // early return in initializer, return 'this' instead below
                                break;
                            } else {
                                self.env = orig_env;
                                return Ok(value);
                            }
                        }

                        // any other error
                        Err(err) => {
                            self.env = orig_env;
                            return Err(err);
                        }
                        _ => {}
                    }
                }

                self.env = orig_env;

                if f.declaration.is_init {
                    // always return 'this' from initializer
                    let this = f.closure.borrow().get_at(0, 0).expect("'this' not found in environment of initializer.");
                    assert!(matches!(this, Value::Instance(_)), "'this' should refer to an instance - got: {}", this);
                    Ok(this)
                } else {
                    Ok(Value::Nil)
                }
            }

            Callable::Builtin(f) => f.call(args),

            Callable::Class(class) => {
                if let Some(init) = class.find_method("init") {
                    assert!(
                        init.declaration.is_init,
                        "Method named 'init' on class should always be recognized as an initializer."
                    );
                    let init = init.clone().bind(Instance::new(class));
                    self.call(Value::Callable(Callable::Function(init)), args) // initializers always returns 'this'
                } else {
                    let instance = Instance::new(class);
                    Ok(Value::Instance(instance))
                }
            }
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
            Stmt::Nop => {}

            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
            }

            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                self.print(&val);
            }

            Stmt::Var { name, init, .. } => {
                let val = if let Some(expr) = init { self.evaluate(expr)? } else { Value::Nil };
                self.env.borrow_mut().define(name, val);
            }

            Stmt::Block(stmts) => {
                let orig_env = self.enter(Rc::clone(&self.env));
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
                            kind: ErrorKind::BreakLoop, ..
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

            Stmt::Function { name, params, body, line } => {
                let fcn = Function::new(Some(name.to_string()), params, body, *line, false, &self.env);
                let fcn_val = Value::Callable(Callable::Function(fcn));

                self.env.borrow_mut().define(name, fcn_val);
            }

            Stmt::Class {
                name,
                superclass,
                methods,
                line,
            } => {
                // ensure class name is defined in current scope
                self.env.borrow_mut().define(name, Value::Nil);

                let superclass: Option<Rc<Class>> = if let Some(superclass) = superclass {
                    let superclass_val = self.evaluate(superclass)?;
                    match superclass_val {
                        Value::Callable(Callable::Class(c)) => Some(c),
                        _ => {
                            return Err(Error::runtime_error("Superclass must be a class."));
                        }
                    }
                } else {
                    None
                };

                // optionally enter a new scope for superclasses, and provide the environment to return to
                let orig_env = if let Some(ref superclass) = superclass {
                    let orig_env = self.enter(Rc::clone(&self.env));
                    let superclass_val = Value::Callable(Callable::Class(Rc::clone(superclass)));
                    self.env.borrow_mut().define("super", superclass_val);
                    Some(orig_env)
                } else {
                    None
                };

                let class = Class::new(name, superclass, methods, *line, &self.env);
                let class_val = Value::Callable(Callable::Class(Rc::new(class)));

                if let Some(orig_env) = orig_env {
                    self.env = orig_env;
                }

                self.env.borrow_mut().define(name, class_val);
            }

            Stmt::Return(expr) => {
                let val = if let Some(expr) = expr { self.evaluate(expr)? } else { Value::Nil };

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
        self.env = Environment::new(env);
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
