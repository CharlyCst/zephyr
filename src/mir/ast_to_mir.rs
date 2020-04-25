use super::mir::{Block, Function, Local, Program};
use super::types::Type;
use super::TypedProgram;
use crate::error::ErrorHandler;

pub struct MIRProducer {
    error_handler: ErrorHandler,
}

impl MIRProducer {
    pub fn new() -> MIRProducer {
        MIRProducer {
            error_handler: ErrorHandler::new(),
        }
    }

    pub fn produce(&mut self, prog: TypedProgram) -> Program {
        Program {
            funs: vec![Function {
                ident: String::from("test"),
                param_types: vec![Type::I32, Type::F32],
                ret_types: vec![Type::I32],
                locals: vec![Local { id: 0 }, Local { id: 1 }],
                blocks: vec![Block { id: 0 }, Block { id: 1 }],
            }],
        }
    }
}
