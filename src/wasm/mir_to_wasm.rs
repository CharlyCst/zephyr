use super::opcode::*;
use super::sections;
use super::wasm;
use crate::error::ErrorHandler;
use crate::hir;
use crate::mir;

use std::collections::HashMap;

// Map element IDs to final wasm IDs
type LocalsMap = HashMap<mir::LocalId, usize>;
type BlocksMap = HashMap<mir::BasicBlockId, usize>;
type FunctionsMap = HashMap<hir::FunId, usize>;
type OffsetMap = HashMap<hir::DataId, wasm::Offset>;

/// State globally availlable, which contains functions and global variables.
struct GlobalState {
    funs: FunctionsMap,
    offsets: OffsetMap,
}

impl GlobalState {
    pub fn new(
        funs: &Vec<mir::Function>,
        imports: &Vec<mir::Imports>,
        offsets: OffsetMap,
    ) -> GlobalState {
        let mut fun_map = HashMap::new();
        let mut fun_idx = 0;
        for import in imports {
            for proto in &import.prototypes {
                fun_map.insert(proto.fun_id, fun_idx);
                fun_idx += 1;
            }
        }
        for (idx, fun) in funs.iter().enumerate() {
            fun_map.insert(fun.fun_id, idx + fun_idx);
        }
        GlobalState {
            funs: fun_map,
            offsets,
        }
    }
}

/// A state that is local to a function or block.
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
            global_state,
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

    pub fn get_fun(&self, fun_id: hir::FunId) -> usize {
        self.global_state.funs[&fun_id]
    }
}

/// Convert MIR to the final wasm output.
pub struct Compiler<'a> {
    err: &'a mut ErrorHandler,
}

impl<'a> Compiler<'a> {
    pub fn new(error_handler: &mut ErrorHandler) -> Compiler {
        Compiler { err: error_handler }
    }

    pub fn compile(&mut self, mir: mir::Program) -> Vec<Instr> {
        let (data_section, offsets) = self.initialize_data(mir.data);
        let global_state = GlobalState::new(&mir.funs, &mir.imports, offsets);
        let mut funs = Vec::new();
        let mut imports = Vec::new();
        for fun in mir.funs {
            funs.push(self.function(fun, &global_state));
        }
        for module_imports in mir.imports {
            imports.extend(self.module_imports(module_imports));
        }

        let module = sections::Module::new(funs, imports, data_section);
        module.encode()
    }

    fn initialize_data(
        &self,
        mir_data: HashMap<mir::DataId, mir::Data>,
    ) -> (sections::SectionData, OffsetMap) {
        let mut data_section = sections::SectionData::new();
        let mut offsets = HashMap::with_capacity(mir_data.len());
        for (data_id, data) in mir_data {
            let offset = data_section.add_data_segment(data);
            offsets.insert(data_id, offset);
        }
        (data_section, offsets)
    }

    /// Compiles a set of MIR module imports to a list of wasm imports.
    fn module_imports(&mut self, module_imports: mir::Imports) -> Vec<wasm::Import> {
        let mut imports = Vec::with_capacity(module_imports.prototypes.len());
        for import in module_imports.prototypes {
            imports.push(self.import(module_imports.from.clone(), import));
        }
        imports
    }

    fn import(&mut self, module: String, proto: mir::FunctionPrototype) -> wasm::Import {
        let mut params = Vec::new();
        let mut results = Vec::new();

        for param in proto.param_t.iter() {
            let t = mir_t_to_wasm(*param);
            params.push(t);
        }

        for ret in proto.ret_t.iter() {
            let t = mir_t_to_wasm(*ret);
            results.push(t);
        }

        wasm::Import {
            module,
            name: proto.ident,
            param_types: params,
            ret_types: results,
            kind: KIND_FUNC,
            type_idx: std::usize::MAX,
        }
    }

    /// Compiles a MIR function down to wasm.
    fn function(&mut self, fun: mir::Function, gs: &GlobalState) -> wasm::Function {
        let mut params = Vec::new();
        let mut results = Vec::new();
        let mut state = LocalState::new(gs);

        for param in fun.param_t.iter() {
            let t = mir_t_to_wasm(*param);
            params.push(t);
        }

        for ret in fun.ret_t.iter() {
            let t = mir_t_to_wasm(*ret);
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
            fun_id: *gs.funs.get(&fun.fun_id).unwrap() as u64,
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
        code.extend(to_leb(fun.locals.len() as u64));
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
                mir::Statement::Local(local) => match local {
                    mir::Local::Set(l_id) => {
                        let local_idx = s.locals[&l_id];
                        code.push(INSTR_LOCAL_SET);
                        code.extend(to_leb(local_idx as u64));
                    }
                    mir::Local::Get(l_id) => {
                        let local_idx = s.locals[&l_id];
                        code.push(INSTR_LOCAL_GET);
                        code.extend(to_leb(local_idx as u64));
                    }
                },
                mir::Statement::Const(val) => match val {
                    mir::Value::I32(x) => {
                        code.push(INSTR_I32_CST);
                        code.extend(to_sleb(x as i64));
                    }
                    mir::Value::I64(x) => {
                        code.push(INSTR_I64_CST);
                        code.extend(to_sleb(x));
                    }
                    mir::Value::F32(x) => {
                        code.push(INSTR_F32_CST);
                        code.extend(x.to_le_bytes().iter());
                    }
                    mir::Value::F64(x) => {
                        code.push(INSTR_F64_CST);
                        code.extend(x.to_le_bytes().iter());
                    }
                    mir::Value::DataPointer(data_id) => {
                        let offset = *s.global_state.offsets.get(&data_id).unwrap();
                        code.push(INSTR_I32_CST);
                        code.extend(to_sleb(offset as i64));
                    }
                },
                mir::Statement::Control(cntrl) => match cntrl {
                    mir::Control::Return => code.push(INSTR_RETURN),
                    mir::Control::Unreachable => code.push(INSTR_UNREACHABLE),
                    mir::Control::Br(label) => {
                        code.push(INSTR_BR);
                        code.extend(to_leb(s.get_label(label) as u64));
                    }
                    mir::Control::BrIf(label) => {
                        code.push(INSTR_BR_IF);
                        code.extend(to_leb(s.get_label(label) as u64));
                    }
                },
                mir::Statement::Block(block) => self.block(*block, s, code),
                mir::Statement::Binop(binop) => code.push(get_binop(binop)),
                mir::Statement::Unop(unop) => code.push(get_unop(unop)),
                mir::Statement::Relop(relop) => code.push(get_relop(relop)),
                mir::Statement::Call(call) => match call {
                    mir::Call::Direct(fun_id) => {
                        code.push(INSTR_CALL);
                        code.extend(to_leb(s.get_fun(fun_id) as u64));
                    }
                    mir::Call::Indirect() => self
                        .err
                        .report_internal_no_loc(String::from("Indirect call not yet implemented")),
                },
                mir::Statement::Parametric(param) => match param {
                    mir::Parametric::Drop => code.push(INSTR_DROP),
                },
                mir::Statement::Memory(mem) => match mem {
                    mir::Memory::Size => {
                        code.push(INSTR_MEMORY_SIZE);
                        code.push(0x00);
                    }
                    mir::Memory::Grow => {
                        code.push(INSTR_MEMORY_GROW);
                        code.push(0x00);
                    }
                    mir::Memory::I32Load { align, offset } => {
                        code.push(INSTR_I32_LOAD);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I64Load { align, offset } => {
                        code.push(INSTR_I64_LOAD);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::F32Load { align, offset } => {
                        code.push(INSTR_F32_LOAD);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::F64Load { align, offset } => {
                        code.push(INSTR_F64_LOAD);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I32Load8u { align, offset } => {
                        code.push(INSTR_I32_LOAD8_U);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I64Load8u { align, offset } => {
                        code.push(INSTR_I64_LOAD8_U);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I32Store { align, offset } => {
                        code.push(INSTR_I32_STORE);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I64Store { align, offset } => {
                        code.push(INSTR_I64_STORE);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::F32Store { align, offset } => {
                        code.push(INSTR_F32_STORE);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::F64Store { align, offset } => {
                        code.push(INSTR_F64_STORE);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I32Store8 { align, offset } => {
                        code.push(INSTR_I32_STORE8);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                    mir::Memory::I64Store8 { align, offset } => {
                        code.push(INSTR_I64_STORE8);
                        code.extend(to_leb(align as u64));
                        code.extend(to_leb(offset as u64));
                    }
                },
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
        mir::Binop::I32And => INSTR_I32_AND,
        mir::Binop::I32Or => INSTR_I32_OR,

        mir::Binop::I64Add => INSTR_I64_ADD,
        mir::Binop::I64Sub => INSTR_I64_SUB,
        mir::Binop::I64Mul => INSTR_I64_MUL,
        mir::Binop::I64Div => INSTR_I64_DIV_U,
        mir::Binop::I64Rem => INSTR_I64_REM_U,
        mir::Binop::I64Xor => INSTR_I64_XOR,
        mir::Binop::I64And => INSTR_I64_AND,
        mir::Binop::I64Or => INSTR_I64_OR,

        mir::Binop::F32Add => INSTR_F32_ADD,
        mir::Binop::F32Sub => INSTR_F32_SUB,
        mir::Binop::F32Mul => INSTR_F32_MUL,
        mir::Binop::F32Div => INSTR_F32_DIV,

        mir::Binop::F64Add => INSTR_F64_ADD,
        mir::Binop::F64Sub => INSTR_F64_SUB,
        mir::Binop::F64Mul => INSTR_F64_MUL,
        mir::Binop::F64Div => INSTR_F64_DIV,
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

        mir::Relop::F32Eq => INSTR_F32_EQ,
        mir::Relop::F32Ne => INSTR_F32_NE,
        mir::Relop::F32Lt => INSTR_F32_LT,
        mir::Relop::F32Gt => INSTR_F32_GT,
        mir::Relop::F32Le => INSTR_F32_LE,
        mir::Relop::F32Ge => INSTR_F32_GE,

        mir::Relop::F64Eq => INSTR_F64_EQ,
        mir::Relop::F64Ne => INSTR_F64_NE,
        mir::Relop::F64Lt => INSTR_F64_LT,
        mir::Relop::F64Gt => INSTR_F64_GT,
        mir::Relop::F64Le => INSTR_F64_LE,
        mir::Relop::F64Ge => INSTR_F64_GE,
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
