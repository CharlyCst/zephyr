use super::types::Type;
use std::fmt;

pub struct Program {
    pub funs: Vec<Function>,
}

pub struct Function {
    pub ident: String,
    pub param_types: Vec<Type>,
    pub ret_types: Vec<Type>,
    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    pub exported: bool,
}

pub type LocalId = usize; // For now NameId are used as LocalId

pub struct Local {
    pub id: LocalId,
}

pub type BasicBlockId = usize;

pub struct BasicBlock {
    pub id: BasicBlockId,
    pub stmts: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> BasicBlock {
        BasicBlock {
            id: id,
            stmts: vec![],
            terminator: None,
        }
    }
}

pub enum Statement {
    Set { l_id: LocalId },
    Get { l_id: LocalId },
    Const { val: Value },
}

pub enum Terminator {
    Return,
    Goto(BasicBlockId),
}

pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
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
        let blocks = self
            .blocks
            .iter()
            .map(|b| format!("{}", b))
            .collect::<Vec<String>>()
            .join("\n");
        write!(
            f,
            "    {}({}) {} {{\n{}{}    }}",
            self.ident, params, ret, locals, blocks
        )
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut block = format!("        bb{} {{\n", self.id);
        for stmt in &self.stmts {
            block.push_str(&format!("            {}\n", stmt))
        }
        if let Some(term) = &self.terminator {
            block.push_str(&format!("            {}\n", term))
        }
        block.push_str("        }\n");
        write!(f, "{}", block)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Const { val } => match val {
                Value::I32(x) => write!(f, "i32 {}", x),
                Value::I64(x) => write!(f, "i64 {}", x),
                Value::F32(x) => write!(f, "f32 {}", x),
                Value::F64(x) => write!(f, "f64 {}", x),
            },
            Statement::Get { l_id } => write!(f, "get _{}", l_id),
            Statement::Set { l_id } => write!(f, "set _{}", l_id),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Goto(bb_id) => write!(f, "goto bb{}", bb_id),
            Terminator::Return => write!(f, "return"),
        }
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "        _{}\n", self.id)
    }
}
