use super::opcode;

pub struct Function {
    pub param_types: Vec<Type>,
    pub ret_types: Vec<Type>,
    pub type_idx: usize, // Used by encode
    pub body: Vec<opcode::Instr>,
    pub export_name: Option<String>,
}

#[derive(Copy, Clone)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}
