use std::fmt::{Display, Formatter};

pub enum AstNode {
    Expr(Expr),
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
    Group(Box<Expr>), // needed only to print out expected format
}

pub enum Literal {
    Nil,
    True,
    False,
    Number(f64),
    String(String),
}

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Not,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            AstNode::Expr(expr) => expr.fmt(f),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expr::Literal(literal) => literal.fmt(f),
            Expr::Group(expr) => write!(f, "(group {})", expr),
            Expr::UnaryExpr { op, right } => write!(f, "({} {})", op, right),
            Expr::BinaryExpr { op, left, right } => write!(f, "({} {} {})", op, left, right),
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
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Not => "!",
            Op::Neg => "-",
            Op::Eq => "==",
            Op::Ne => "!=",
            Op::Lt => "<",
            Op::Le => "<=",
            Op::Gt => ">",
            Op::Ge => ">=",
        };
        write!(f, "{}", s)
    }
}
