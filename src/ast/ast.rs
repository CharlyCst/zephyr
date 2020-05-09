use crate::error::Location;
use std::fmt;

pub enum Value {
    Integer { val: u64, loc: Location },
    Boolean { val: bool, loc: Location },
}

#[derive(Copy, Clone)]
pub enum BinaryOperator {
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Plus,
    Minus,
    Multiply,
    Remainder,
    Divide,
    BitwiseOr,
    BitwiseAnd,
    Or,
    And,
}

#[derive(Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Not,
}

pub struct Variable {
    pub ident: String,
    pub t: Option<String>,
    pub loc: Location,
}

pub enum Expression {
    Variable {
        var: Variable,
    },
    Literal {
        value: Value,
    },
    Binary {
        expr_left: Box<Expression>,
        binop: BinaryOperator,
        expr_right: Box<Expression>,
    },
    Unary {
        unop: UnaryOperator,
        expr: Box<Expression>,
    },
    Call {
        fun: Box<Expression>,
        args: Vec<Expression>,
    },
}

pub enum Statement {
    ExprStmt {
        expr: Box<Expression>,
    },
    LetStmt {
        var: Box<Variable>,
        expr: Box<Expression>,
    },
    AssignStmt {
        var: Box<Variable>,
        expr: Box<Expression>,
    },
    IfStmt {
        expr: Box<Expression>,
        block: Block,
    },
    WhileStmt {
        expr: Box<Expression>,
        block: Block,
    },
    ReturnStmt {
        expr: Option<Expression>,
        loc: Location,
    },
}

pub struct Function {
    pub ident: String,
    pub params: Vec<Variable>,
    pub result: Option<(String, Location)>,
    pub block: Block,
    pub exported: bool,
    pub loc: Location,
}

pub struct Block {
    pub stmts: Vec<Statement>,
}

pub struct Program {
    pub funs: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut program = String::from("");
        for stmt in &self.funs {
            program.push_str(&format!("{}\n", stmt));
        }
        write!(f, "{}", program)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if self.exported { "export " } else { "" };
        let params = self
            .params
            .iter()
            .map(|v| {
                let mut param = v.ident.clone();
                param.push_str(" ");
                param.push_str(match v.t {
                    Some(ref typ) => typ,
                    None => "?",
                });
                param
            })
            .collect::<Vec<String>>()
            .join(", ");
        let result_type = if let Some((ref t, _)) = self.result {
            let mut t = t.clone();
            t.push_str(" ");
            t
        } else {
            String::from("")
        };
        write!(
            f,
            "{}{}({}) {}{};",
            prefix, self.ident, params, result_type, self.block
        )
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut strs = Vec::new();
        strs.push(String::from("{"));
        for stmt in self.stmts.iter() {
            for line in format!("{}", stmt).split("\n") {
                let mut indented_line = String::from("    ");
                indented_line.push_str(line);
                strs.push(indented_line)
            }
        }
        strs.push(String::from("}"));
        write!(
            f,
            "{}",
            strs.iter().map(|s| &**s).collect::<Vec<&str>>().join("\n"),
        )
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Variable { var: v, .. } => write!(f, "{}", v.ident),
            Expression::Literal { value: v } => match v {
                Value::Boolean { val: true, .. } => write!(f, "true"),
                Value::Boolean { val: false, .. } => write!(f, "false"),
                Value::Integer { val: n, .. } => write!(f, "{}", n),
            },
            Expression::Call { fun, args } => write!(
                f,
                "{}({})",
                fun,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Unary { unop, expr } => match unop {
                UnaryOperator::Not => write!(f, "!{}", expr),
                UnaryOperator::Minus => write!(f, "-{}", expr),
            },
            Expression::Binary {
                expr_left,
                binop,
                expr_right,
            } => {
                let op = match binop {
                    BinaryOperator::And => "&&",
                    BinaryOperator::BitwiseAnd => "&",
                    BinaryOperator::BitwiseOr => "|",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Equal => "==",
                    BinaryOperator::Greater => ">",
                    BinaryOperator::GreaterEqual => ">=",
                    BinaryOperator::Less => "<",
                    BinaryOperator::LessEqual => "<=",
                    BinaryOperator::Minus => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Remainder => "%",
                    BinaryOperator::NotEqual => "!=",
                    BinaryOperator::Or => "||",
                    BinaryOperator::Plus => "+",
                };
                write!(f, "({} {} {})", expr_left, op, expr_right)
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::ExprStmt { expr } => write!(f, "{};", expr),
            Statement::LetStmt { var, expr } => write!(f, "let {} = {};", var.ident, expr),
            Statement::AssignStmt { var, expr } => write!(f, "{} = {};", var.ident, expr),
            Statement::IfStmt { expr, block } => write!(f, "if {} {};", expr, block),
            Statement::WhileStmt { expr, block } => write!(f, "while {} {};", expr, block),
            Statement::ReturnStmt { expr, .. } => match expr {
                Some(e) => write!(f, "return {};", e),
                None => write!(f, "return;"),
            },
        }
    }
}