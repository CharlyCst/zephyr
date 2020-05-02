use super::opcode::*;
use super::sections;
use super::wasm;
use crate::error::ErrorHandler;
use crate::mir;

use std::collections::HashMap;

type LocalsMap = HashMap<mir::LocalId, usize>;

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

        let module = sections::Module::new(funs);
        module.encode()
    }

    fn function(&mut self, fun: mir::Function) -> wasm::Function {
        let mut params = Vec::new();
        let mut results = Vec::new();
        let mut locals = HashMap::new();

        for param in fun.param_types.iter() {
            let t = mir_t_to_wasm(*param);
            params.push(t);
        }

        for param in fun.ret_types.iter() {
            let t = mir_t_to_wasm(*param);
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
        self.locals(&fun, &mut locals, &mut code);
        self.body(fun.body, &locals, &mut code);
        code.push(INSTR_END);

        wasm::Function {
            param_types: params,
            ret_types: results,
            type_idx: std::usize::MAX,
            body: code,
            export_name: export_name,
        }
    }

    fn locals(&mut self, fun: &mir::Function, locals_map: &mut LocalsMap, code: &mut Vec<Instr>) {
        let mut local_decl = Vec::new();
        let mut idx = 0;
        for param in &fun.params {
            locals_map.insert(*param, idx);
            idx += 1;
        }
        for local in &fun.locals {
            let t = mir_t_to_wasm(local.t);
            local_decl.push(0x1); // TODO: compress locals of same types
            local_decl.push(type_to_bytes(t));
            locals_map.insert(local.id, idx);
            idx += 1;
        }
        code.extend(to_leb(fun.locals.len()));
        code.extend(local_decl);
    }

    fn body(&mut self, block: mir::Block, locals_map: &LocalsMap, code: &mut Vec<Instr>) {
        match block {
            mir::Block::Block { id, stmts } => {
                self.statements(stmts, locals_map, code);
            }
            _ => self
                .error_handler
                .report_internal("The body of a function must by a Block::Block"),
        }
    }

    fn block(&mut self, block: &mir::Block, locals_map: &LocalsMap, code: &mut Vec<Instr>) {
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

    fn statements(
        &mut self,
        stmts: Vec<mir::Statement>,
        locals_map: &LocalsMap,
        code: &mut Vec<Instr>,
    ) {
        for stmt in stmts {
            match stmt {
                mir::Statement::Set { l_id } => {
                    let local_idx = locals_map[&l_id];
                    code.push(INSTR_LOCAL_SET);
                    code.extend(to_leb(local_idx));
                }
                mir::Statement::Get { l_id } => {
                    let local_idx = locals_map[&l_id];
                    code.push(INSTR_LOCAL_GET);
                    code.extend(to_leb(local_idx));
                }
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
                mir::Statement::Binop { binop } => match binop {
                    mir::Binop::I32Add => code.push(INSTR_I32_ADD),
                    _ => self
                        .error_handler
                        .report_internal("Binop not yet implemented"),
                },
                _ => self
                    .error_handler
                    .report_internal("Statement not yet implemented"),
            }
        }
    }
}

fn mir_t_to_wasm(t: mir::Type) -> wasm::Type {
    match t {
        mir::Type::I32 => wasm::Type::I32,
        mir::Type::I64 => wasm::Type::I64,
        mir::Type::F32 => wasm::Type::F32,
        mir::Type::F64 => wasm::Type::F64,
    }
}
