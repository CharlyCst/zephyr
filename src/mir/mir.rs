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
    Unop { unop: Unop },
    Binop { binop: Binop },
    Relop { relop: Relop },
    Parametric { param: Parametric },
}

pub enum Terminator {
    Return,
    Goto(BasicBlockId),
    BrIf(BasicBlockId, BasicBlockId), // true, false
}

pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

pub enum Unop {
    I32Neg,
    I64Neg,
    F32Neg,
    F64Neg,
}

pub enum Binop {
    I32Xor,
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,

    I64Add,
    I64Sub,
    I64Mul,
    I64Div,

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

pub enum Parametric {
    Drop,
}

pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Fun(Vec<Type>, Vec<Type>),
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
            Statement::Unop { unop } => write!(f, "{}", unop),
            Statement::Binop { binop } => write!(f, "{}", binop),
            Statement::Relop { relop } => write!(f, "{}", relop),
            Statement::Parametric { param } => write!(f, "{}", param),
        }
    }
}

impl fmt::Display for Unop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unop::I32Neg => write!(f, "i32.ne"),
            Unop::I64Neg => write!(f, "i64.ne"),
            Unop::F32Neg => write!(f, "f32.ne"),
            Unop::F64Neg => write!(f, "f64.ne"),
        }
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binop::I32Xor => write!(f, "i32.xor"),
            Binop::I32Add => write!(f, "i32.add"),
            Binop::I32Sub => write!(f, "i32.sub"),
            Binop::I32Mul => write!(f, "i32.mul"),
            Binop::I32Div => write!(f, "i32.div"),

            Binop::I64Add => write!(f, "i64.add"),
            Binop::I64Sub => write!(f, "i64.sub"),
            Binop::I64Mul => write!(f, "i64.mul"),
            Binop::I64Div => write!(f, "i64.div"),

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

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return => write!(f, "return"),
            Terminator::Goto(bb_id) => write!(f, "goto bb{}", bb_id),
            Terminator::BrIf(tbb, fbb) => write!(f, "br_if bb{}, bb{}", tbb, fbb),
        }
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "        _{}\n", self.id)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Fun(params, results) => {
                let p_types = params
                    .iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<String>>()
                    .join(", ");
                let r_types = results
                    .iter()
                    .map(|result| format!("{}", result))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fun({}) {}", p_types, r_types)
            }
        }
    }
}
