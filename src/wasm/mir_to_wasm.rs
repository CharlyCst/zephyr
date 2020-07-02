use super::opcode::*;
use super::sections;
use super::wasm;
use crate::error::ErrorHandler;
use crate::mir;

use std::collections::HashMap;

type LocalsMap = HashMap<mir::LocalId, usize>;
type BlocksMap = HashMap<mir::BasicBlockId, usize>;
type FunctionsMap = HashMap<mir::FunctionId, usize>;

struct GlobalState {
    funs: FunctionsMap,
}

impl GlobalState {
    pub fn new(funs: &Vec<mir::Function>) -> GlobalState {
        let mut fun_map = HashMap::new();
        for (idx, fun) in funs.iter().enumerate() {
            fun_map.insert(fun.fun_id, idx);
        }
        GlobalState { funs: fun_map }
    }
}

struct LocalState<'a> {
    locals: LocalsMap,
    blocks: BlocksMap,
    depth: usize,
    global_state: &'a GlobalState,
}

impl<'a> LocalState<'a> {
    pub fn new(global_state: &GlobalState) -> LocalState {
        LocalState {
            locals: HashMap::new(),
            blocks: HashMap::new(),
            global_state: global_state,
            depth: 0,
        }
    }
    pub fn block_start(&mut self, label: mir::BasicBlockId) {
        self.blocks.insert(label, self.depth);
        self.depth += 1;
    }

    pub fn block_end(&mut self) {
        self.depth -= 1;
    }

    pub fn get_label(&self, label: mir::BasicBlockId) -> usize {
        self.depth - self.blocks[&label] - 1
    }

    pub fn get_fun(&self, fun_id: mir::FunctionId) -> usize {
        self.global_state.funs[&fun_id]
    }
}

pub struct Compiler<'a> {
    err: &'a mut ErrorHandler,
}

impl<'a> Compiler<'a> {
    pub fn new(error_handler: &mut ErrorHandler) -> Compiler {
        Compiler { err: error_handler }
    }

    pub fn compile(&mut self, mir: mir::Program) -> Vec<Instr> {
        let global_state = GlobalState::new(&mir.funs);
        let mut funs = Vec::new();
        for fun in mir.funs {
            funs.push(self.function(fun, &global_state));
        }

        let module = sections::Module::new(funs);
        module.encode()
    }

    fn function(&mut self, fun: mir::Function, gs: &GlobalState) -> wasm::Function {
        let mut params = Vec::new();
        let mut results = Vec::new();
        let mut state = LocalState::new(gs);

        for param in fun.param_types.iter() {
            let t = mir_t_to_wasm(*param);
            params.push(t);
        }

        for param in fun.ret_types.iter() {
            let t = mir_t_to_wasm(*param);
            results.push(t);
        }

        let mut code = Vec::new();
        self.locals(&fun, &mut state.locals, &mut code);
        self.body(fun.body, &mut state, &mut code);
        code.push(INSTR_END);

        wasm::Function {
            param_types: params,
            ret_types: results,
            type_idx: std::usize::MAX,
            exposed: fun.exposed,
            body: code,
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

    fn body(&mut self, block: mir::Block, s: &mut LocalState, code: &mut Vec<Instr>) {
        match block {
            mir::Block::Block { stmts, id, .. } => {
                s.block_start(id);
                self.statements(stmts, s, code);
                s.block_end();
            }
            _ => self.err.report_internal_no_loc(String::from(
                "The body of a function must by a Block::Block",
            )),
        }
    }

    fn block(&mut self, block: mir::Block, s: &mut LocalState, code: &mut Vec<Instr>) {
        match block {
            mir::Block::Block { stmts, id, t } => {
                s.block_start(id);
                code.push(INSTR_BLOCK);
                if let Some(t) = t {
                    code.push(type_to_bytes(mir_t_to_wasm(t)));
                } else {
                    code.push(BLOCK_TYPE);
                }
                self.statements(stmts, s, code);
                code.push(INSTR_END);
                s.block_end();
            }
            mir::Block::Loop { stmts, id, t } => {
                s.block_start(id);
                code.push(INSTR_LOOP);
                if let Some(t) = t {
                    code.push(type_to_bytes(mir_t_to_wasm(t)));
                } else {
                    code.push(BLOCK_TYPE);
                }
                self.statements(stmts, s, code);
                code.push(INSTR_END);
                s.block_end();
            }
            mir::Block::If {
                then_stmts,
                else_stmts,
                id,
                t,
            } => {
                s.block_start(id);
                code.push(INSTR_IF);
                if let Some(t) = t {
                    code.push(type_to_bytes(mir_t_to_wasm(t)));
                } else {
                    code.push(BLOCK_TYPE);
                }
                self.statements(then_stmts, s, code);
                if else_stmts.len() > 0 {
                    code.push(INSTR_ELSE);
                    self.statements(else_stmts, s, code);
                }
                code.push(INSTR_END);
                s.block_end();
            }
        }
    }

    fn statements(
        &mut self,
        stmts: Vec<mir::Statement>,
        s: &mut LocalState,
        code: &mut Vec<Instr>,
    ) {
        for stmt in stmts {
            match stmt {
                mir::Statement::Set { l_id } => {
                    let local_idx = s.locals[&l_id];
                    code.push(INSTR_LOCAL_SET);
                    code.extend(to_leb(local_idx));
                }
                mir::Statement::Get { l_id } => {
                    let local_idx = s.locals[&l_id];
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
                        .err
                        .report_internal_no_loc(String::from("Floating points not yet supported")),
                    mir::Value::F64(_) => self
                        .err
                        .report_internal_no_loc(String::from("Floating points not yet supported")),
                },
                mir::Statement::Control { cntrl } => match cntrl {
                    mir::Control::Return => code.push(INSTR_RETURN),
                    mir::Control::Br(label) => {
                        code.push(INSTR_BR);
                        code.extend(to_leb(s.get_label(label)));
                    }
                    mir::Control::BrIf(label) => {
                        code.push(INSTR_BR_IF);
                        code.extend(to_leb(s.get_label(label)));
                    }
                },
                mir::Statement::Block { block } => self.block(*block, s, code),
                mir::Statement::Binop { binop } => code.push(get_binop(binop)),
                mir::Statement::Unop { unop } => code.push(get_unop(unop)),
                mir::Statement::Relop { relop } => code.push(get_relop(relop)),
                mir::Statement::Call { call } => match call {
                    mir::Call::Direct(fun_id) => {
                        code.push(INSTR_CALL);
                        code.extend(to_leb(s.get_fun(fun_id)));
                    }
                    mir::Call::Indirect() => self
                        .err
                        .report_internal_no_loc(String::from("Indirect call not yet implemented")),
                },
                _ => self
                    .err
                    .report_internal_no_loc(String::from("Statement not yet implemented")),
            }
        }
    }
}

fn get_binop(binop: mir::Binop) -> Instr {
    match binop {
        mir::Binop::I32Add => INSTR_I32_ADD,
        mir::Binop::I32Sub => INSTR_I32_SUB,
        mir::Binop::I32Mul => INSTR_I32_MUL,
        mir::Binop::I32Div => INSTR_I32_DIV_U,
        mir::Binop::I32Rem => INSTR_I32_REM_U,
        mir::Binop::I32Xor => INSTR_I32_XOR,

        mir::Binop::I64Add => INSTR_I64_ADD,
        mir::Binop::I64Sub => INSTR_I64_SUB,
        mir::Binop::I64Mul => INSTR_I64_MUL,
        mir::Binop::I64Div => INSTR_I64_DIV_U,
        mir::Binop::I64Rem => INSTR_I64_REM_U,

        _ => unimplemented!(),
    }
}

fn get_unop(unop: mir::Unop) -> Instr {
    match unop {
        // These instructions do not exist for I32 and I64, as:
        //
        //   > There is no distinction between signed and unsigned
        //   > integer types. Instead, integers are interpreted by
        //   > respective operations as either unsigned or signed
        //   > in two’s complement representation.
        //
        // https://www.w3.org/TR/wasm-core-1/#concepts%E2%91%A0
        mir::Unop::F32Neg => INSTR_F32_NEG,
        mir::Unop::F64Neg => INSTR_F64_NEG,
    }
}

fn get_relop(relop: mir::Relop) -> Instr {
    match relop {
        mir::Relop::I32Eq => INSTR_I32_EQ,
        mir::Relop::I32Ne => INSTR_I32_NE,
        mir::Relop::I32Lt => INSTR_I32_LT_S,
        mir::Relop::I32Gt => INSTR_I32_GT_S,
        mir::Relop::I32Le => INSTR_I32_LE_S,
        mir::Relop::I32Ge => INSTR_I32_GE_S,

        mir::Relop::I64Eq => INSTR_I64_EQ,
        mir::Relop::I64Ne => INSTR_I64_NE,
        mir::Relop::I64Lt => INSTR_I64_LT_S,
        mir::Relop::I64Gt => INSTR_I64_GT_S,
        mir::Relop::I64Le => INSTR_I64_LE_S,
        mir::Relop::I64Ge => INSTR_I64_GE_S,

        _ => unimplemented!(),
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
