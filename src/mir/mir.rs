#![allow(dead_code)] // Call::Indirect, Value::F32, Value::F64
use crate::hir::{Declaration as HirDeclaration, FunId, LocalId};

use std::collections::HashMap;
use std::fmt;

pub struct Program {
    pub name: String,
    pub funs: Vec<Function>,
    pub pub_decls: HashMap<String, HirDeclaration>,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<LocalId>,
    pub param_t: Vec<Type>,
    pub ret_t: Vec<Type>,
    pub locals: Vec<LocalVariable>,
    pub body: Block,
    pub is_pub: bool,
    pub exposed: Option<String>,
    pub fun_id: FunId,
}

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
    Get(LocalId),
    Set(LocalId),
}

pub enum Call {
    Direct(FunId),
    Indirect(),
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
    I32Xor,
    I32Or,
    I32And,
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Rem,

    I64Xor,
    I64Or,
    I64And,
    I64Add,
    I64Sub,
    I64Mul,
    I64Div,
    I64Rem,

    F32Add,
    F32Sub,
    F32Mul,
    F32Div,

    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
}

pub enum Relop {
    I32Eq,
    I32Ne,
    I32Lt,
    I32Gt,
    I32Le,
    I32Ge,

    I64Eq,
    I64Ne,
    I64Lt,
    I64Gt,
    I64Le,
    I64Ge,

    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
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

#[derive(Copy, Clone)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
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
            .param_t
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let ret = self
            .ret_t
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
            Local::Get(l_id) => write!(f, "local.get {}", l_id),
            Local::Set(l_id) => write!(f, "local.set {}", l_id),
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
            Binop::I32Xor => write!(f, "i32.xor"),
            Binop::I32Or => write!(f, "i32.or"),
            Binop::I32And => write!(f, "i32.and"),
            Binop::I32Add => write!(f, "i32.add"),
            Binop::I32Sub => write!(f, "i32.sub"),
            Binop::I32Mul => write!(f, "i32.mul"),
            Binop::I32Div => write!(f, "i32.div"),
            Binop::I32Rem => write!(f, "i32.rem"),

            Binop::I64Xor => write!(f, "i64.xor"),
            Binop::I64Or => write!(f, "i64.or"),
            Binop::I64And => write!(f, "i64.and"),
            Binop::I64Add => write!(f, "i64.add"),
            Binop::I64Sub => write!(f, "i64.sub"),
            Binop::I64Mul => write!(f, "i64.mul"),
            Binop::I64Div => write!(f, "i64.div"),
            Binop::I64Rem => write!(f, "i64.rem"),

            Binop::F32Add => write!(f, "f32.add"),
            Binop::F32Sub => write!(f, "f32.sub"),
            Binop::F32Mul => write!(f, "f32.mul"),
            Binop::F32Div => write!(f, "f32.div"),

            Binop::F64Add => write!(f, "f64.add"),
            Binop::F64Sub => write!(f, "f64.sub"),
            Binop::F64Mul => write!(f, "f64.mul"),
            Binop::F64Div => write!(f, "f64.div"),
        }
    }
}

impl fmt::Display for Relop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Relop::I32Eq => write!(f, "i32.eq"),
            Relop::I32Ne => write!(f, "i32.ne"),
            Relop::I32Lt => write!(f, "i32.lt"),
            Relop::I32Gt => write!(f, "i32.gt"),
            Relop::I32Le => write!(f, "i32.le"),
            Relop::I32Ge => write!(f, "i32.ge"),

            Relop::I64Eq => write!(f, "i64.eq"),
            Relop::I64Ne => write!(f, "i64.ne"),
            Relop::I64Lt => write!(f, "i64.lt"),
            Relop::I64Gt => write!(f, "i64.gt"),
            Relop::I64Le => write!(f, "i64.le"),
            Relop::I64Ge => write!(f, "i64.ge"),

            Relop::F32Eq => write!(f, "f32.eq"),
            Relop::F32Ne => write!(f, "f32.ne"),
            Relop::F32Lt => write!(f, "f32.lt"),
            Relop::F32Gt => write!(f, "f32.gt"),
            Relop::F32Le => write!(f, "f32.le"),
            Relop::F32Ge => write!(f, "f32.ge"),

            Relop::F64Eq => write!(f, "f64.eq"),
            Relop::F64Ne => write!(f, "f64.ne"),
            Relop::F64Lt => write!(f, "f64.lt"),
            Relop::F64Gt => write!(f, "f64.gt"),
            Relop::F64Le => write!(f, "f64.le"),
            Relop::F64Ge => write!(f, "f64.ge"),
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
            Call::Direct(id) => write!(f, "call {}", id),
            Call::Indirect() => write!(f, "call_indirect"),
        }
    }
}

impl fmt::Display for LocalVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    _{}\n", self.id)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
        }
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
