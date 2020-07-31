use crate::error::Location;
use super::asm_statements::AsmStatement;
use std::fmt;

pub enum Value {
    Integer { val: u64, loc: Location },
    Float { val: f64, loc: Location },
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
    Access {
        namespace: Box<Expression>,
        field: Box<Expression>,
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
        else_block: Option<Block>,
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

pub enum Declaration {
    Function(Function),
    Use(Use),
    Expose(Expose),
}

pub struct Program {
    pub package: Package,
    pub funs: Vec<Function>,
    pub exposed: Vec<Expose>,
    pub used: Vec<Use>,
    pub package_id: u32,
}

pub struct Package {
    pub path: String,
    pub loc: Location,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<Variable>,
    pub result: Option<(String, Location)>,
    pub body: Body,
    pub is_pub: bool,
    pub loc: Location,
}

pub struct Expose {
    pub ident: String,
    pub alias: Option<String>,
    pub loc: Location,
}

#[derive(Clone)]
pub struct Use {
    pub path: String,
    pub alias: Option<String>,
    pub loc: Location,
}

pub struct Block {
    pub stmts: Vec<Statement>,
}

pub enum Body {
    Zephyr(Block),
    Asm(Vec<AsmStatement>),
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut program = String::from("");
        // Package
        program.push_str(&format!("packge \"{}\";\n\n", self.package.path));
        // Use
        for is_used in &self.used {
            program.push_str(&format!("use \"{}\"", is_used.path));
            if let Some(ref alias) = is_used.alias {
                program.push_str(&format!(" as {}", alias));
            }
            program.push_str(";\n");
        }
        if self.used.len() > 0 {
            program.push_str("\n");
        }
        // Expose
        for expose in &self.exposed {
            program.push_str(&format!("expose {}", expose.ident));
            if let Some(ref alias) = expose.alias {
                program.push_str(&format!(" as {}", alias));
            }
            program.push_str(";\n");
        }
        if self.exposed.len() > 0 {
            program.push_str("\n");
        }
        // Fun
        for stmt in &self.funs {
            program.push_str(&format!("{}\n", stmt));
        }
        write!(f, "{}", program)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if self.is_pub { "pub " } else { "" };
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
            prefix, self.ident, params, result_type, self.body
        )
    }
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Body::Zephyr(block) => write!(f, "{}", block),
            Body::Asm(stmts) => {
                let mut body = String::from("{\n");
                for stmt in stmts {
                    body.push_str(&format!("    {}\n", stmt));
                }
                body.push_str("}");
                write!(f, "{}", body)
            },
        }
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
                Value::Float { val: x, .. } => write!(f, "{}", x),
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
            Expression::Access { namespace, field } => write!(f, "({}.{})", namespace, field),
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
            Statement::IfStmt {
                expr,
                block,
                else_block,
            } => {
                if let Some(else_block) = else_block {
                    write!(f, "if {} {} else {};", expr, block, else_block)
                } else {
                    write!(f, "if {} {};", expr, block)
                }
            }
            Statement::WhileStmt { expr, block } => write!(f, "while {} {};", expr, block),
            Statement::ReturnStmt { expr, .. } => match expr {
                Some(e) => write!(f, "return {};", e),
                None => write!(f, "return;"),
            },
        }
    }
}
