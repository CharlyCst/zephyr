use super::opcode::*;
use super::sections;
use super::wasm;
use crate::error::ErrorHandler;
use crate::mir;

pub struct Compiler {
    error_handler: ErrorHandler,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            error_handler: ErrorHandler::new(),
        }
    }

    pub fn compile(&mut self, mir: mir::Program) -> Vec<Instr> {
        let mut funs = Vec::new();
        for fun in mir.funs {
            funs.push(self.function(fun));
        }

        let mut module = sections::Module::new(funs);
        module.encode()
    }

    fn function(&mut self, fun: mir::Function) -> wasm::Function {
        let mut params = Vec::new();
        let mut results = Vec::new();

        for param in fun.param_types.iter() {
            let t = match param {
                mir::Type::I32 => wasm::Type::I32,
                mir::Type::I64 => wasm::Type::I64,
                mir::Type::F32 => wasm::Type::F32,
                mir::Type::F64 => wasm::Type::F64,
                _ => {
                    self.error_handler
                        .report_internal("Function parameters include a function type");
                    wasm::Type::I32
                }
            };
            params.push(t);
        }

        for param in fun.ret_types.iter() {
            let t = match param {
                mir::Type::I32 => wasm::Type::I32,
                mir::Type::I64 => wasm::Type::I64,
                mir::Type::F32 => wasm::Type::F32,
                mir::Type::F64 => wasm::Type::F64,
                _ => {
                    self.error_handler
                        .report_internal("Function results include a function type");
                    wasm::Type::I32
                }
            };
            results.push(t);
        }

        let export_name = if fun.exported {
            if fun.ident == "Main" {
                Some(String::from("_start")) // WASI main function
            } else {
                if fun.ident == "main" {
                    self.error_handler
                        .report_line(0, "Main function must be capitalized") // TODO report line
                }
                Some(fun.ident.clone())
            }
        } else {
            None
        };

        let mut code = Vec::new();
        code.extend(to_leb(fun.locals.len())); // local count
        self.body(fun.body, &mut code);
        code.push(INSTR_END);

        wasm::Function {
            param_types: params,
            ret_types: results,
            type_idx: std::usize::MAX,
            body: code,
            export_name: export_name,
        }
    }

    fn body(&mut self, block: mir::Block, code: &mut Vec<Instr>) {
        match block {
            mir::Block::Block { id, stmts } => {
                self.statements(stmts, code);
            }
            _ => self
                .error_handler
                .report_internal("The body of a function must by a Block::Block"),
        }
    }

    fn block(&mut self, block: &mir::Block, code: &mut Vec<Instr>) {
        match block {
            mir::Block::Block { id, stmts } => {}
            mir::Block::Loop { id, stmts } => {}
            mir::Block::If {
                id,
                then_stmts,
                else_stmts,
            } => {}
        }
    }

    fn statements(&mut self, stmts: Vec<mir::Statement>, code: &mut Vec<Instr>) {
        for stmt in stmts {
            match stmt {
                mir::Statement::Set { l_id } => {}
                mir::Statement::Get { l_id } => {}
                mir::Statement::Const { val } => match val {
                    mir::Value::I32(x) => {
                        code.push(INSTR_I32_CST);
                        code.extend(to_leb(x as usize));
                    }
                    mir::Value::I64(x) => {
                        code.push(INSTR_I64_CST);
                        code.extend(to_leb(x as usize));
                    }
                    mir::Value::F32(_) => self
                        .error_handler
                        .report_internal("Floating points not yet supported"),
                    mir::Value::F64(_) => self
                        .error_handler
                        .report_internal("Floating points not yet supported"),
                },
                mir::Statement::Control { cntrl } => match cntrl {
                    mir::Control::Return => code.push(INSTR_RETURN),
                    _ => self
                        .error_handler
                        .report_internal("Control expression not yet implemented"),
                },
                _ => self
                    .error_handler
                    .report_internal("Statement not yet implemented"),
            }
        }
    }
}
