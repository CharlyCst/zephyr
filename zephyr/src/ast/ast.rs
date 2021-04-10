use std::fmt;

pub use crate::ctx::ModId;
use crate::error::Location;
use crate::mir::Value as MirValue;
pub use crate::resolver::ModulePath;

// ——————————————————————————————— Zephyr AST —————————————————————————————— //

/// A module type describes how the module is organised in the filesystem.
#[derive(Clone)]
pub enum ModuleType {
    Standard,
    Standalone,
}

/// A module kind describes the role of the module.
#[derive(Eq, PartialEq, Copy, Clone)]
pub enum ModuleKind {
    Module,
    Runtime,
}

pub enum Value {
    Integer {
        val: u64,
        loc: Location,
    },
    Float {
        val: f64,
        loc: Location,
    },
    Boolean {
        val: bool,
        loc: Location,
    },
    Str {
        val: String,
        loc: Location,
    },
    Struct {
        /// Namespaces are reserved for HIR resolution.
        namespace: Option<ModId>,
        ident: String,
        fields: Vec<FieldValue>,
        loc: Location,
    },
    Tuple {
        values: Vec<Expression>,
        loc: Location,
    },
}

pub struct FieldValue {
    pub ident: String,
    pub expr: Expression,
    pub loc: Location,
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
    BitwiseXor,
    Or,
    And,
}

#[derive(Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Not,
}

pub struct Parameter {
    pub ident: String,
    pub t: Type,
    pub loc: Location,
}

pub struct Variable {
    /// Namespaces are reserver for HIR resolution
    pub namespace: Option<ModId>,
    pub ident: String,
    pub t: Option<String>,
    pub loc: Location,
}

pub enum Expression {
    Variable(Variable),
    Literal(Value),
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
    ExprStmt(Expression),
    LetStmt {
        var: Variable,
        expr: Expression,
    },
    AssignStmt {
        target: Expression,
        expr: Expression,
    },
    IfStmt {
        expr: Expression,
        block: Block,
        else_block: Option<Block>,
    },
    WhileStmt {
        expr: Expression,
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
    Imports(Imports),
    Struct(Struct),
}

pub struct Program {
    pub module: Module,
    pub funs: Vec<Function>,
    pub structs: Vec<Struct>,
    /// Functions exposed to the host runtime.
    pub exposed: Vec<Expose>,
    ///Functions imported from the host runtime.
    pub imports: Vec<Imports>,
    pub used: Vec<Use>,
}

impl Program {
    /// Merges the properties of another AST into this one, this is used for instance when a
    /// module spans multiples files to get back a single AST.
    ///
    /// The `Module` property is kept, it is the responsibility of the caller to ensure that
    /// module are merged in a coherent fashion.
    pub fn merge(&mut self, other: Self) {
        self.funs.extend(other.funs);
        self.structs.extend(other.structs);
        self.exposed.extend(other.exposed);
        self.imports.extend(other.imports);
        self.used.extend(other.used);
    }
}

#[derive(Clone)]
pub struct Module {
    pub id: ModId,
    pub name: String,
    pub loc: Location,
    pub t: ModuleType,
    pub kind: ModuleKind,
}

pub struct Imports {
    pub from: String,
    pub prototypes: Vec<FunctionPrototype>,
    pub loc: Location,
}

pub struct Struct {
    pub ident: String,
    pub fields: Vec<StructField>,
    pub is_pub: bool,
    pub loc: Location,
}

pub struct StructField {
    pub is_pub: bool,
    pub ident: String,
    pub t: Type,
    pub loc: Location,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<Parameter>,
    pub result: Option<Type>,
    pub body: Body,
    pub is_pub: bool,
    pub loc: Location,
}

pub struct FunctionPrototype {
    pub ident: String,
    pub alias: Option<String>,
    pub params: Vec<Parameter>,
    pub result: Option<Type>,
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
    pub path: ModulePath,
    pub alias: Option<String>,
    pub loc: Location,
}

pub struct Path {
    pub root: String,
    pub path: Vec<String>,
    pub loc: Location,
}

pub enum Type {
    Simple(Path),
    Tuple(Vec<Type>, Location),
}

impl Type {
    pub fn get_loc(&self) -> Location {
        match self {
            Type::Simple(path) => path.loc,
            Type::Tuple(_, loc) => *loc,
        }
    }
}

pub struct Block {
    pub stmts: Vec<Statement>,
}

pub enum Body {
    Zephyr(Block),
    Asm(Vec<AsmStatement>),
}

// ——————————————————————————————— Zephyr ASM —————————————————————————————— //

pub enum AsmStatement {
    Local { local: AsmLocal, loc: Location },
    Const { val: MirValue, loc: Location },
    Control { cntrl: AsmControl, loc: Location },
    Parametric { param: AsmParametric, loc: Location },
    Memory { mem: AsmMemory, loc: Location },
}

pub enum AsmLocal {
    Get { ident: String, loc: Location },
    Set { ident: String, loc: Location },
}

pub enum AsmMemory {
    Size,
    Grow,
    I32Load { align: u32, offset: u32 },
    I64Load { align: u32, offset: u32 },
    F32Load { align: u32, offset: u32 },
    F64Load { align: u32, offset: u32 },
    I32Load8u { align: u32, offset: u32 },
    I32Store { align: u32, offset: u32 },
    I64Store { align: u32, offset: u32 },
    F32Store { align: u32, offset: u32 },
    F64Store { align: u32, offset: u32 },
    I32Store8 { align: u32, offset: u32 },
}

pub enum AsmControl {
    Return,
    Unreachable,
}

pub enum AsmParametric {
    Drop,
}

// ———————————————————————————————— Display ———————————————————————————————— //

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut program = String::from("");
        // Package
        program.push_str(&format!("packge \"{}\";\n\n", self.module.name));
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
                param.push_str(&format!("{}", v.t));
                param
            })
            .collect::<Vec<String>>()
            .join(", ");
        let result_type = if let Some(ref t) = self.result {
            format!("{} ", t)
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
            }
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
            Expression::Variable(v) => write!(f, "{}", v.ident),
            Expression::Literal(v) => match v {
                Value::Boolean { val: true, .. } => write!(f, "true"),
                Value::Boolean { val: false, .. } => write!(f, "false"),
                Value::Integer { val: n, .. } => write!(f, "{}", n),
                Value::Float { val: x, .. } => write!(f, "{}", x),
                Value::Str { val: s, .. } => write!(f, "{}", s),
                Value::Struct { ident, fields, .. } => write!(
                    f,
                    "{} {{ {} }}",
                    ident,
                    fields
                        .iter()
                        .map(|FieldValue { ident, expr, .. }| format!("{}: {}", ident, expr))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Value::Tuple { values, .. } => write!(
                    f,
                    "({})",
                    values
                        .iter()
                        .map(|exp| format!("{}", exp))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
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
                    BinaryOperator::BitwiseXor => "^",
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
            Statement::ExprStmt(expr) => write!(f, "{};", expr),
            Statement::LetStmt { var, expr } => write!(f, "let {} = {};", var.ident, expr),
            Statement::AssignStmt { target, expr } => write!(f, "{} = {};", target, expr),
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

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut path = self.root.clone();
        for access in &self.path {
            path.push('.');
            path.push_str(access);
        }
        write!(f, "{}", path)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Simple(t) => write!(f, "{}", t),
            Type::Tuple(types, _) => {
                let types = types
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "({})", types)
            }
        }
    }
}

impl fmt::Display for AsmStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmStatement::Local { local, .. } => write!(f, "{}", local),
            AsmStatement::Const { val, .. } => write!(f, "{}", val),
            AsmStatement::Control { cntrl, .. } => write!(f, "{}", cntrl),
            AsmStatement::Parametric { param, .. } => write!(f, "{}", param),
            AsmStatement::Memory { mem, .. } => write!(f, "{}", mem),
        }
    }
}

impl fmt::Display for AsmLocal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmLocal::Get { ident, .. } => write!(f, "local.get {}", ident),
            AsmLocal::Set { ident, .. } => write!(f, "local.set {}", ident),
        }
    }
}

impl fmt::Display for AsmControl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmControl::Return => write!(f, "return"),
            AsmControl::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl fmt::Display for AsmParametric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmParametric::Drop => write!(f, "drop"),
        }
    }
}

impl fmt::Display for AsmMemory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmMemory::Size => write!(f, "memory.size"),
            AsmMemory::Grow => write!(f, "memory.grow"),
            AsmMemory::I32Load { align, offset } => write!(f, "i32.load {}, {}", align, offset),
            AsmMemory::I64Load { align, offset } => write!(f, "i64.load {}, {}", align, offset),
            AsmMemory::F32Load { align, offset } => write!(f, "f32.load {}, {}", align, offset),
            AsmMemory::F64Load { align, offset } => write!(f, "f64.load {}, {}", align, offset),
            AsmMemory::I32Load8u { align, offset } => {
                write!(f, "i32.load8_u {}, {}", align, offset)
            }
            AsmMemory::I32Store { align, offset } => write!(f, "i32.store {}, {}", align, offset),
            AsmMemory::I64Store { align, offset } => write!(f, "i64.store {}, {}", align, offset),
            AsmMemory::F32Store { align, offset } => write!(f, "f32.store {}, {}", align, offset),
            AsmMemory::F64Store { align, offset } => write!(f, "f64.store {}, {}", align, offset),
            AsmMemory::I32Store8 { align, offset } => write!(f, "i32.store8 {}, {}", align, offset),
        }
    }
}
