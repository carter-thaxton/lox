use std::fmt::{Display, Formatter};

pub type Program = Vec<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Nop,
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Var {
        name: String,
        init: Option<Expr>,
        line: usize,
    },
    Block(Vec<Stmt>),
    IfElse {
        cond: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Stmt>,
    },
    Function {
        name: String,
        params: Vec<(String, usize)>,
        body: Vec<Stmt>,
        line: usize,
    },
    Class {
        name: String,
        superclass: Option<Box<Expr>>, // Expr::Variable
        methods: Vec<Stmt>,            // all Stmt::Function
        line: usize,
    },
    Return(Option<Box<Expr>>),
    Break,
    Continue,

    ExpectOutput(String),
    ExpectRuntimeError(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    UnaryExpr {
        op: Op,
        right: Box<Expr>,
    },
    BinaryExpr {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Group(Box<Expr>), // parentheses - needed only to print out expected format
    Variable {
        name: String,
        line: usize,
        depth_and_index: Option<(usize, usize)>,
    },
    Assign {
        name: String,
        right: Box<Expr>,
        line: usize,
        depth_and_index: Option<(usize, usize)>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        line: usize,
    },
    Function {
        params: Vec<(String, usize)>,
        body: Vec<Stmt>,
        line: usize,
    },
    Get {
        object: Box<Expr>,
        property: String,
        line: usize,
    },
    Set {
        object: Box<Expr>,
        property: String,
        value: Box<Expr>,
        line: usize,
    },
    This {
        line: usize,
        depth_and_index: Option<(usize, usize)>,
    },
    Super {
        method: String,
        line: usize,
        depth_and_index: Option<(usize, usize)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Not,
    Neg,
    Mul,
    Div,
    Add,
    Sub,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Group(expr) => write!(f, "(group {})", expr),
            Expr::UnaryExpr { op, right } => write!(f, "({} {})", op, right),
            Expr::BinaryExpr { op, left, right } => write!(f, "({} {} {})", op, left, right),
            Expr::Variable { name, .. } => write!(f, "(var {})", name),
            Expr::Assign { name, right, .. } => write!(f, "({} = {})", name, right),
            Expr::Call { callee, args, .. } => {
                // (call <callee> (<arg1>, .. <argN>))
                write!(f, "(call {} (", callee)?;
                let mut first = true;
                for arg in args {
                    if !first {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", arg)?;
                    first = false;
                }
                write!(f, "))")
            }
            Expr::Function { params, .. } => {
                // (fun (<param1>, .. <paramN>))
                write!(f, "(fun (")?;
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", param.0)?;
                    first = false;
                }
                write!(f, "))")
            }
            Expr::Get { object, property, .. } => {
                write!(f, "(get {}.{})", object, property)
            }
            Expr::Set { object, property, value, .. } => {
                write!(f, "(set {}.{} = {})", object, property, value)
            }
            Expr::This { .. } => {
                write!(f, "(this)")
            }
            Expr::Super { method, .. } => {
                write!(f, "(super {})", method)
            }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Literal::Nil => {
                write!(f, "nil")
            }
            Literal::True => {
                write!(f, "true")
            }
            Literal::False => {
                write!(f, "false")
            }
            Literal::Number(n) => {
                if *n == n.trunc() && !n.is_infinite() && !n.is_nan() {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Literal::String(s) => {
                write!(f, "{}", s) // very bad format for literals.  should be quoted ""
            }
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let s = match self {
            Op::Not => "!",
            Op::Neg => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Add => "+",
            Op::Sub => "-",
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::And => "and",
            Op::Or => "or",
        };
        write!(f, "{}", s)
    }
}
