#![allow(dead_code)] // Call::Indirect
use super::names::{AsmStatement, NameId};
use crate::driver::PackageDeclarations;
use crate::error::Location;

use std::fmt;

const TYPE_I32: Type = Type::Scalar(ScalarType::I32);
const TYPE_I64: Type = Type::Scalar(ScalarType::I64);
const TYPE_F32: Type = Type::Scalar(ScalarType::F32);
const TYPE_F64: Type = Type::Scalar(ScalarType::F64);
const TYPE_BOOL: Type = Type::Scalar(ScalarType::Bool);

pub use super::names::{FunId, StructId};
pub use crate::ast::Package;
pub type LocalId = usize; // For now NameId are used as LocalId
pub type BasicBlockId = usize;

#[derive(Clone)]
pub enum Type {
    Scalar(ScalarType),
    Fun(FunctionType),
    Tuple(TupleType),
    Struct(StructId),
}

#[derive(Clone, Copy)]
pub enum ScalarType {
    I32,
    I64,
    F32,
    F64,
    Bool,
}

#[derive(Clone, Copy)]
pub enum IntegerType {
    I32,
    I64,
}

#[derive(Clone, Copy)]
pub enum NumericType {
    I32,
    I64,
    F32,
    F64,
}

pub type TupleType = Vec<Type>;

#[derive(Clone)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub ret: TupleType,
}

impl FunctionType {
    pub fn new(args: Vec<Type>, ret: Vec<Type>) -> Self {
        FunctionType { params: args, ret }
    }
}

pub struct Program {
    pub funs: Vec<Function>,
    pub imports: Vec<Imports>,
    pub pub_decls: PackageDeclarations,
    pub package: Package,
}

pub struct Imports {
    pub from: String,
    pub prototypes: Vec<FunctionPrototype>,
    pub loc: Location,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<LocalId>,
    pub t: FunctionType,
    pub locals: Vec<LocalVariable>,
    pub body: Body,
    pub loc: Location,
    pub is_pub: bool,
    pub exposed: Option<String>,
    pub fun_id: FunId,
}

pub struct FunctionPrototype {
    pub ident: String,
    pub t: FunctionType,
    pub alias: Option<String>,
    pub is_pub: bool,
    pub loc: Location,
    pub fun_id: FunId,
}

pub struct LocalVariable {
    pub id: LocalId,
    pub t: Type,
    pub loc: Location,
}

pub enum Body {
    Zephyr(Block),
    Asm(Vec<AsmStatement>),
}

pub struct Block {
    pub stmts: Vec<Statement>,
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

pub struct Variable {
    pub ident: String,
    pub loc: Location,
    pub n_id: NameId,
    pub t: Type,
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
        binop: Binop,
        expr_right: Box<Expression>,
        loc: Location,
    },
    Unary {
        unop: Unop,
        expr: Box<Expression>,
        loc: Location,
    },
    CallDirect {
        fun_id: FunId,
        t: FunctionType,
        args: Vec<Expression>,
        loc: Location,
    },
    CallIndirect {
        fun: Box<Expression>,
        args: Vec<Expression>,
        t: FunctionType,
        loc: Location,
    },
    Access {
        expr: Box<Expression>,
        field: String,
        t: Type,
        loc: Location,
    },
    Nop {
        loc: Location,
    },
}

pub enum Local {
    Get {
        local_id: LocalId,
        t: Type,
        loc: Location,
    },
    Set {
        local_id: LocalId,
        t: Type,
        loc: Location,
    },
}

pub enum Call {
    Direct {
        fun_id: FunId,
        fun_t: FunctionType,
        loc: Location,
    },
    Indirect {
        fun_t: FunctionType,
        loc: Location,
    },
}

pub enum Control {
    Return,
    Unreachable,
    Br(BasicBlockId),
    BrIf(BasicBlockId),
}

pub enum Value {
    I32(i32, Location),
    I64(i64, Location),
    F32(f32, Location),
    F64(f64, Location),
    Bool(bool, Location),
    Struct {
        struct_id: StructId,
        fields: Vec<FieldValue>,
        loc: Location,
    },
}

pub struct FieldValue {
    pub ident: String,
    pub expr: Box<Expression>,
    pub loc: Location,
}

/// The available unary operations, type represents operant type.
pub enum Unop {
    Neg(NumericType),
    // Boolean
    Not,
}

/// The available binary operations, type represents operands type.
pub enum Binop {
    Add(NumericType),
    Sub(NumericType),
    Mul(NumericType),
    Div(NumericType),

    Rem(IntegerType),
    Xor(IntegerType),
    BinaryAnd(IntegerType),
    BinaryOr(IntegerType),

    Eq(ScalarType),
    Ne(ScalarType),
    Lt(NumericType),
    Gt(NumericType),
    Le(NumericType),
    Ge(NumericType),

    // Boolean
    LogicalAnd,
    LogicalOr,
}

pub enum Parametric {
    Drop,
}

pub enum Memory {
    Size,
    Grow,
    I32Load { align: u32, offset: u32 },
    I64Load { align: u32, offset: u32 },
    F32Load { align: u32, offset: u32 },
    F64Load { align: u32, offset: u32 },
    I32Store { align: u32, offset: u32 },
    I64Store { align: u32, offset: u32 },
    F32Store { align: u32, offset: u32 },
    F64Store { align: u32, offset: u32 },
}

impl Expression {
    pub fn get_loc(&self) -> Location {
        match self {
            Expression::Variable { var } => var.loc,
            Expression::Literal { value } => match value {
                Value::F64(_, loc) => *loc,
                Value::F32(_, loc) => *loc,
                Value::I64(_, loc) => *loc,
                Value::I32(_, loc) => *loc,
                Value::Bool(_, loc) => *loc,
                Value::Struct { loc, .. } => *loc,
            },
            Expression::Unary { loc, .. } => *loc,
            Expression::Binary { loc, .. } => *loc,
            Expression::CallDirect { loc, .. } => *loc,
            Expression::CallIndirect { loc, .. } => *loc,
            Expression::Access { loc, .. } => *loc,
            Expression::Nop { loc } => *loc,
        }
    }
}

impl Unop {
    pub fn get_t(&self) -> ScalarType {
        match self {
            Unop::Not => ScalarType::Bool,
            Unop::Neg(t) => t.get_t(),
        }
    }
}

impl Binop {
    pub fn get_t(&self) -> ScalarType {
        match self {
            Binop::LogicalAnd => ScalarType::Bool,
            Binop::LogicalOr => ScalarType::Bool,
            Binop::BinaryAnd(t) => t.get_t(),
            Binop::BinaryOr(t) => t.get_t(),
            Binop::Xor(t) => t.get_t(),
            Binop::Rem(t) => t.get_t(),
            Binop::Ge(t) => t.get_t(),
            Binop::Gt(t) => t.get_t(),
            Binop::Le(t) => t.get_t(),
            Binop::Lt(t) => t.get_t(),
            Binop::Ne(t) => *t,
            Binop::Eq(t) => *t,
            Binop::Div(t) => t.get_t(),
            Binop::Mul(t) => t.get_t(),
            Binop::Sub(t) => t.get_t(),
            Binop::Add(t) => t.get_t(),
        }
    }
}

impl IntegerType {
    pub fn get_t(&self) -> ScalarType {
        match self {
            IntegerType::I32 => ScalarType::I32,
            IntegerType::I64 => ScalarType::I64,
        }
    }
}

impl NumericType {
    pub fn get_t(&self) -> ScalarType {
        match self {
            NumericType::I32 => ScalarType::I32,
            NumericType::I64 => ScalarType::I64,
            NumericType::F32 => ScalarType::F32,
            NumericType::F64 => ScalarType::F64,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Scalar(t) => write!(f, "{}", t),
            Type::Fun(t) => write!(f, "{}", t),
            Type::Struct(s_id) => write!(f, "struct #{}", s_id),
            Type::Tuple(t) => write!(
                f,
                "({})",
                t.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScalarType::F64 => write!(f, "f64"),
            ScalarType::F32 => write!(f, "f32"),
            ScalarType::I64 => write!(f, "i64"),
            ScalarType::I32 => write!(f, "i32"),
            ScalarType::Bool => write!(f, "bool"),
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self
            .params
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let ret = self
            .ret
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "({}) -> ({})", args, ret)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let funs = self
            .funs
            .iter()
            .map(|fun| format!("{}", fun))
            .collect::<Vec<String>>()
            .join("\n\n");
        write!(f, "MIR {{\n{}\n}}", funs)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .t
            .params
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let ret = self
            .t
            .ret
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let locals = self
            .locals
            .iter()
            .map(|l| format!("{}", l))
            .collect::<Vec<String>>()
            .join("");
        let mut body = Vec::new();
        for line in format!("{}", self.body).split("\n") {
            let mut indented_line = String::from("    ");
            indented_line.push_str(line);
            body.push(indented_line)
        }
        write!(
            f,
            "  {}({}) {} {{\n{}{}\n  }}",
            self.ident,
            params,
            ret,
            locals,
            body.iter().map(|s| &**s).collect::<Vec<&str>>().join("\n")
        )
    }
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Body::Zephyr(block) => write!(f, "{}", block),
            Body::Asm(stmts) => {
                let mut strs = Vec::new();
                for stmt in stmts {
                    strs.push(format!("{}", stmt));
                }
                write!(
                    f,
                    "{}",
                    strs.iter().map(|s| &**s).collect::<Vec<&str>>().join("\n"),
                )
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
            Expression::Variable { var: v, .. } => write!(f, "{}", v.ident),
            Expression::Literal { value: v } => write!(f, "{}", v),
            Expression::CallDirect { fun_id, args, .. } => write!(
                f,
                "(fun {})({})",
                fun_id,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::CallIndirect { fun, args, .. } => write!(
                f,
                "{}({})",
                fun,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Unary { unop, expr, .. } => match unop {
                Unop::Not => write!(f, "!{}", expr),
                Unop::Neg(_) => write!(f, "-{}", expr),
            },
            Expression::Binary {
                expr_left,
                binop,
                expr_right,
                ..
            } => write!(f, "({} {} {})", expr_left, binop, expr_right),
            Expression::Access { expr, field, .. } => write!(f, "{}.{}", expr, field),
            Expression::Nop { .. } => write!(f, "nop"),
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

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Local::Get { local_id, .. } => write!(f, "local.get {}", local_id),
            Local::Set { local_id, .. } => write!(f, "local.set {}", local_id),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I32(x, _) => write!(f, "i32.const {}", x),
            Value::I64(x, _) => write!(f, "i64.const {}", x),
            Value::F32(x, _) => write!(f, "f32.const {}", x),
            Value::F64(x, _) => write!(f, "f64.const {}", x),
            Value::Bool(x, _) => {
                if *x {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Value::Struct {
                struct_id, fields, ..
            } => write!(
                f,
                "struct #{} {{ {} }}",
                struct_id,
                fields
                    .iter()
                    .map(|f| f.ident.as_str())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for Unop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unop::Not => write!(f, "!"),
            Unop::Neg(_) => write!(f, "-"),
        }
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let repr = match self {
            Binop::LogicalAnd => "&&",
            Binop::LogicalOr => "||",
            Binop::BinaryAnd(_) => "&",
            Binop::BinaryOr(_) => "|",
            Binop::Xor(_) => "^",
            Binop::Eq(_) => "==",
            Binop::Ne(_) => "!=",
            Binop::Gt(_) => ">",
            Binop::Ge(_) => ">=",
            Binop::Lt(_) => "<",
            Binop::Le(_) => "<=",
            Binop::Add(_) => "+",
            Binop::Sub(_) => "-",
            Binop::Mul(_) => "*",
            Binop::Div(_) => "/",
            Binop::Rem(_) => "%",
        };
        write!(f, "{}", repr)
    }
}

impl fmt::Display for Parametric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Parametric::Drop => write!(f, "drop"),
        }
    }
}

impl fmt::Display for Control {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Control::Return => write!(f, "return"),
            Control::Unreachable => write!(f, "unreachable"),
            Control::Br(bb_id) => write!(f, "br {}", bb_id),
            Control::BrIf(bb_id) => write!(f, "br_if {}", bb_id),
        }
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Call::Direct { fun_id, .. } => write!(f, "call {}", fun_id),
            Call::Indirect { .. } => write!(f, "call_indirect"),
        }
    }
}

impl fmt::Display for LocalVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    _{}\n", self.id)
    }
}

impl fmt::Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Memory::Size => write!(f, "memory.size"),
            Memory::Grow => write!(f, "memory.grow"),
            Memory::I32Load { align, offset } => write!(f, "i32.load {}, {}", align, offset),
            Memory::I64Load { align, offset } => write!(f, "i64.load {}, {}", align, offset),
            Memory::F32Load { align, offset } => write!(f, "f32.load {}, {}", align, offset),
            Memory::F64Load { align, offset } => write!(f, "f64.load {}, {}", align, offset),
            Memory::I32Store { align, offset } => write!(f, "i32.store {}, {}", align, offset),
            Memory::I64Store { align, offset } => write!(f, "i64.store {}, {}", align, offset),
            Memory::F32Store { align, offset } => write!(f, "f32.store {}, {}", align, offset),
            Memory::F64Store { align, offset } => write!(f, "f64.store {}, {}", align, offset),
        }
    }
}
