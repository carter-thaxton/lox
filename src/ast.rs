use std::fmt::{Display, Formatter};

pub type Program = Vec<Stmt>;

pub enum Stmt {
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),

    ExpectOutput(String),
    ExpectRuntimeError(String),
}

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
    Variable(String),
    Assign {
        name: String,
        right: Box<Expr>,
    },
}

pub enum Literal {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
}

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
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Group(expr) => write!(f, "(group {})", expr),
            Expr::UnaryExpr { op, right } => write!(f, "({} {})", op, right),
            Expr::BinaryExpr { op, left, right } => write!(f, "({} {} {})", op, left, right),
            Expr::Variable(name) => write!(f, "(var {})", name),
            Expr::Assign { name, right } => write!(f, "({} = {})", name, right),
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
        };
        write!(f, "{}", s)
    }
}
