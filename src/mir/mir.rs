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
    pub blocks: Vec<Block>,
    pub exported: bool,
}

pub struct Local {
    pub id: usize,
}

pub struct Block {
    pub id: usize,
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

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut block = format!("        bb{} {{\n", self.id);
        block.push_str("        }\n");
        write!(f, "{}", block)
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "        _{}\n", self.id)
    }
}
