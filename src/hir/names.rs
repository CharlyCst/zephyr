use super::types::{ConstraintStore, Type, TypeId, TypeVarStore};
use crate::ast::{BinaryOperator, Package, UnaryOperator};
use crate::ctx::ModId;
use crate::error::Location;
use crate::mir::Value as MirValue;
use std::collections::HashMap;
use std::fmt;

pub use crate::ast::{AsmControl, AsmMemory, AsmParametric};

pub type NameId = usize;
pub type FunId = u64;
pub type StructId = u64;
pub type DataId = u64;
pub type TypeNamespace = HashMap<StructId, Struct>;

/// A resolved program, ready to be typechecked.
pub struct ResolvedProgram {
    pub funs: Vec<Function>,
    pub imports: Vec<Imports>,
    pub data: HashMap<DataId, Data>,
    pub structs: HashMap<StructId, Struct>,
    pub names: NameStore,
    pub types: TypeVarStore,
    pub constraints: ConstraintStore,
    pub package: Package,
}

/// All the kind of values that can be found in the Value Namespace.
pub enum ValueKind {
    Function(FunId, NameId),
    Module(ModId),
}

/// All the kind of types that can be found in the Type Namespace.
pub enum TypeKind {
    Struct(StructId),
}

pub enum Data {
    Str(DataId, Vec<u8>),
}

pub struct Imports {
    pub from: String,
    pub prototypes: Vec<FunctionPrototype>,
    pub loc: Location,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<Variable>,
    pub locals: Vec<NameId>,
    pub body: Body,
    pub is_pub: bool,
    pub exposed: Option<String>,
    pub loc: Location,
    pub n_id: NameId,
    pub fun_id: FunId,
}

pub struct FunctionPrototype {
    pub ident: String,
    pub n_id: NameId,
    pub fun_id: FunId,
    pub alias: Option<String>,
    pub is_pub: bool,
    pub loc: Location,
}

pub struct Struct {
    pub ident: String,
    pub s_id: StructId,
    pub fields: HashMap<String, StructField>,
    pub is_pub: bool,
    pub loc: Location,
}

pub struct StructField {
    pub is_pub: bool,
    pub t: Type,
    pub loc: Location,
}

#[derive(Clone)]
pub enum ValueDeclaration {
    Function { t: Type, fun_id: FunId },
    Module(ModId),
}

#[derive(Clone)]
pub enum TypeDeclaration {
    Struct(StructId),
}

pub enum Body {
    Zephyr(Block),
    Asm(Vec<AsmStatement>),
}

pub struct Block {
    pub stmts: Vec<Statement>,
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

pub struct Variable {
    pub ident: String,
    pub loc: Location,
    pub n_id: NameId,
}

pub enum Value {
    Integer {
        val: u64,
        loc: Location,
        t_id: TypeId,
    },
    Float {
        val: f64,
        loc: Location,
        t_id: TypeId,
    },
    Boolean {
        val: bool,
        loc: Location,
        t_id: TypeId,
    },
    Str {
        data_id: DataId,
        len: u64,
        loc: Location,
        t_id: TypeId,
    },
    Struct {
        ident: String,
        loc: Location,
        fields: Vec<FieldValue>,
        t_id: TypeId,
    },
}

pub struct FieldValue {
    pub ident: String,
    pub expr: Box<Expression>,
    pub loc: Location,
    pub t_id: TypeId,
}

pub enum Expression {
    Variable(Variable),
    Literal(Value),
    Function {
        fun_id: FunId,
        loc: Location,
    },
    Access {
        expr: Box<Expression>,
        field: String,
        t_id: TypeId,
        struct_t_id: TypeId,
        loc: Location,
    },
    Namespace {
        mod_id: ModId,
        loc: Location,
    },
    Binary {
        expr_left: Box<Expression>,
        binop: BinaryOperator,
        expr_right: Box<Expression>,
        loc: Location,
        t_id: TypeId,    // Result type
        op_t_id: TypeId, // Operands types
    },
    Unary {
        unop: UnaryOperator,
        expr: Box<Expression>,
        loc: Location,
        op_t_id: TypeId,
    },
    CallDirect {
        fun_id: FunId,
        args: Vec<Expression>,
        loc: Location,
        fun_t_id: TypeId,
        ret_t_id: TypeId,
    },
    CallIndirect {
        fun: Box<Expression>,
        args: Vec<Expression>,
        loc: Location,
        fun_t_id: TypeId,
        ret_t_id: TypeId,
    },
}

impl Expression {
    pub fn get_loc(&self) -> Location {
        match self {
            Expression::Variable(var) => var.loc,
            Expression::Literal(value) => match value {
                Value::Boolean { loc, .. } => *loc,
                Value::Integer { loc, .. } => *loc,
                Value::Float { loc, .. } => *loc,
                Value::Str { loc, .. } => *loc,
                Value::Struct { loc, .. } => *loc,
            },
            Expression::Function { loc, .. } => *loc,
            Expression::Access { loc, .. } => *loc,
            Expression::Namespace { loc, .. } => *loc,
            Expression::Unary { loc, .. } => *loc,
            Expression::Binary { loc, .. } => *loc,
            Expression::CallDirect { loc, .. } => *loc,
            Expression::CallIndirect { loc, .. } => *loc,
        }
    }
}

pub enum AsmStatement {
    Local { local: AsmLocal, loc: Location },
    Const { val: MirValue, loc: Location },
    Control { cntrl: AsmControl, loc: Location },
    Parametric { param: AsmParametric, loc: Location },
    Memory { mem: AsmMemory, loc: Location },
}

pub enum AsmLocal {
    Get { var: Variable },
    Set { var: Variable },
}

impl AsmStatement {
    pub fn _get_loc(&self) -> Location {
        match self {
            AsmStatement::Local { loc, .. } => *loc,
            AsmStatement::Const { loc, .. } => *loc,
            AsmStatement::Control { loc, .. } => *loc,
            AsmStatement::Parametric { loc, .. } => *loc,
            AsmStatement::Memory { loc, .. } => *loc,
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
            AsmLocal::Get { var } => write!(f, "local.get {}", var.ident),
            AsmLocal::Set { var } => write!(f, "local.set {}", var.ident),
        }
    }
}

// Stuff relative to names
pub struct Name {
    pub n_id: NameId,
    pub name: String,
    pub loc: Location,
    pub t_id: TypeId,
}

pub struct NameStore {
    names: Vec<Name>,
}

impl NameStore {
    pub fn new() -> NameStore {
        NameStore { names: Vec::new() }
    }

    pub fn get(&self, id: NameId) -> &Name {
        &self.names[id]
    }

    pub fn fresh(&mut self, name: String, loc: Location, t_id: TypeId) -> NameId {
        let id = self.names.len();
        let n = Name {
            n_id: id,
            name,
            loc,
            t_id,
        };
        self.names.push(n);
        id
    }
}

impl fmt::Display for NameStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("NameStore {\n");
        store.push_str("    id ~ t_id - name\n\n");
        for (idx, name) in self.names.iter().enumerate() {
            store.push_str(&format!(
                "  {:>4} ~ {:>4} - {:}\n",
                idx, name.t_id, name.name
            ));
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}
