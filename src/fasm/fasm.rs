use crate::error::Location;

pub struct Function {
    pub ident: String,
    // pub params: Vec<Variable>,
    // pub result: Option<(String, Location)>,
    // pub block: Block,
    pub exported: bool,
    pub loc: Location,
}

pub struct Program {
    pub funs: Vec<Function>,
}
