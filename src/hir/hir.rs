#![allow(dead_code)] // Call::Indirect, Value::F32, Value::F64
use super::names::Declaration;

use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub enum Type {
    Scalar(ScalarType),
    Fun(FunctionType),
}

#[derive(Clone, Copy)]
pub enum ScalarType {
    I32,
    I64,
    F32,
    F64,
    Bool,
}

#[derive(Clone)]
pub struct FunctionType {
    args: Vec<Type>,
    ret: Vec<Type>,
}

pub struct Program {
    pub funs: Vec<Function>,
    pub pub_decls: HashMap<String, Declaration>,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<LocalId>,
    pub param_types: Vec<Type>,
    pub ret_types: Vec<Type>,
    pub locals: Vec<LocalVariable>,
    pub body: Block,
    pub is_pub: bool,
    pub exposed: Option<String>,
    pub fun_id: FunctionId,
}

pub type LocalId = usize; // For now NameId are used as LocalId
pub type FunctionId = u64;

pub struct LocalVariable {
    pub id: LocalId,
    pub t: Type,
}

pub type BasicBlockId = usize;

pub enum Block {
    Block {
        id: BasicBlockId,
        stmts: Vec<Statement>,
        t: Option<Type>,
    },
    Loop {
        id: BasicBlockId,
        stmts: Vec<Statement>,
        t: Option<Type>,
    },
    If {
        id: BasicBlockId,
        then_stmts: Vec<Statement>,
        else_stmts: Vec<Statement>,
        t: Option<Type>,
    },
}

pub enum Statement {
    Local { local: Local },
    Const { val: Value },
    Block { block: Box<Block> },
    Unop { unop: Unop },
    Binop { binop: Binop },
    Relop { relop: Relop },
    Control { cntrl: Control },
    Call { call: Call },
    Parametric { param: Parametric },
    Memory { mem: Memory },
}

pub enum Local {
    Get { local_id: LocalId, t: Type },
    Set { local_id: LocalId, t: Type },
}

pub enum Call {
    Direct {
        fun_id: FunctionId,
        fun_t: FunctionType,
    },
    Indirect {
        fun_t: FunctionType,
    },
}

pub enum Control {
    Return,
    Unreachable,
    Br(BasicBlockId),
    BrIf(BasicBlockId),
}

pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

pub enum Unop {
    F32Neg,
    F64Neg,
}

pub enum Binop {
    Xor(ScalarType),
    Add(ScalarType),
    Sub(ScalarType),
    Mul(ScalarType),
    Div(ScalarType),
    Rem(ScalarType),
}

pub enum Relop {
    Eq(ScalarType),
    Ne(ScalarType),
    Lt(ScalarType),
    Gt(ScalarType),
    Le(ScalarType),
    Ge(ScalarType),
}

pub enum Logical {
    And,
    Or,
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Scalar(t) => write!(f, "{}", t),
            Type::Fun(t) => write!(f, "{}", t),
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
            .args
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
            .param_types
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let ret = self
            .ret_types
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

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut strs = Vec::new();
        let stmts = match self {
            Block::Block { id, stmts, .. } => {
                strs.push(format!("block {} {{", id));
                stmts
            }
            Block::Loop { id, stmts, .. } => {
                strs.push(format!("loop {} {{", id));
                stmts
            }
            Block::If { id, then_stmts, .. } => {
                strs.push(format!("if {} {{", id));
                then_stmts
            }
        };
        for stmt in stmts.iter() {
            for line in format!("{}", stmt).split("\n") {
                let mut indented_line = String::from("  ");
                indented_line.push_str(line);
                strs.push(indented_line)
            }
        }
        if let Block::If { else_stmts, .. } = self {
            strs.push(String::from("} else {"));
            for stmt in else_stmts.iter() {
                for line in format!("{}", stmt).split("\n") {
                    let mut indented_line = String::from("  ");
                    indented_line.push_str(line);
                    strs.push(indented_line)
                }
            }
        };
        strs.push(String::from("}"));
        write!(
            f,
            "{}",
            strs.iter().map(|s| &**s).collect::<Vec<&str>>().join("\n"),
        )
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Local { local } => write!(f, "{}", local),
            Statement::Unop { unop } => write!(f, "{}", unop),
            Statement::Binop { binop } => write!(f, "{}", binop),
            Statement::Relop { relop } => write!(f, "{}", relop),
            Statement::Parametric { param } => write!(f, "{}", param),
            Statement::Block { block } => write!(f, "{}", block),
            Statement::Control { cntrl } => write!(f, "{}", cntrl),
            Statement::Call { call } => write!(f, "{}", call),
            Statement::Const { val } => write!(f, "{}", val),
            Statement::Memory { mem } => write!(f, "{}", mem),
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
            Value::I32(x) => write!(f, "i32.const {}", x),
            Value::I64(x) => write!(f, "i64.const {}", x),
            Value::F32(x) => write!(f, "f32.const {}", x),
            Value::F64(x) => write!(f, "f64.const {}", x),
        }
    }
}

impl fmt::Display for Unop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unop::F32Neg => write!(f, "f32.ne"),
            Unop::F64Neg => write!(f, "f64.ne"),
        }
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binop::Xor(t) => write!(f, "{}.xor", t),
            Binop::Add(t) => write!(f, "{}.add", t),
            Binop::Sub(t) => write!(f, "{}.sub", t),
            Binop::Mul(t) => write!(f, "{}.mul", t),
            Binop::Div(t) => write!(f, "{}.div", t),
            Binop::Rem(t) => write!(f, "{}.rem", t),
        }
    }
}

impl fmt::Display for Relop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Relop::Eq(t) => write!(f, "{}.eq", t),
            Relop::Ne(t) => write!(f, "{}.ne", t),
            Relop::Lt(t) => write!(f, "{}.lt", t),
            Relop::Gt(t) => write!(f, "{}.gt", t),
            Relop::Le(t) => write!(f, "{}.le", t),
            Relop::Ge(t) => write!(f, "{}.ge", t),
        }
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
