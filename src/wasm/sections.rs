#[warn()]
use std::collections::HashMap;

use super::opcode::*;
use super::wasm;

struct SectionType {
    types: Vec<Vec<Instr>>,
    size: Vec<Instr>,
    count: Vec<Instr>,
}

impl SectionType {
    // Function declaration format:
    // [Func] (nb_args) [arg_1] [arg_2] ... (nb_results) [result_1] [result_2] ...
    fn new(funs: &mut Vec<wasm::Function>) -> SectionType {
        let mut types = Vec::new();
        let mut size = 0;
        let mut index: usize = 0;
        let mut known_types = HashMap::new();

        for fun in funs.iter_mut() {
            let mut params = Vec::new();
            let mut results = Vec::new();
            let mut fun_type = Vec::new();

            for t in fun.param_types.iter() {
                params.push(type_to_bytes(t))
            }

            for t in fun.ret_types.iter() {
                results.push(type_to_bytes(t))
            }

            fun_type.push(Func);
            fun_type.append(&mut to_leb(params.len()));
            fun_type.extend(params);
            fun_type.append(&mut to_leb(results.len()));
            fun_type.extend(results);

            match known_types.get(&fun_type) {
                Some(idx) => {
                    fun.type_idx = *idx;
                }
                None => {
                    size += fun_type.len();
                    known_types.insert(fun_type.clone(), index);

                    types.push(fun_type);
                    fun.type_idx = index;
                    index += 1;
                }
            }
        }

        let count = to_leb(types.len());
        size += count.len();

        SectionType {
            types: types,
            size: to_leb(size),
            count: count,
        }
    }

    fn encode(&self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(SEC_TYPE);
        bytecode.extend(self.size.iter());
        bytecode.extend(self.count.iter());

        // Body
        for fun_type in self.types.iter() {
            bytecode.extend(fun_type);
        }

        bytecode
    }
}

struct SectionFunction {
    types: Vec<Vec<Instr>>,
    size: Vec<Instr>,
    count: Vec<Instr>,
}

impl SectionFunction {
    fn new(funs: &Vec<wasm::Function>) -> SectionFunction {
        let mut types = Vec::new();
        let mut size = 0;

        for fun in funs {
            let idx = to_leb(fun.type_idx);
            size += idx.len();
            types.push(idx);
        }

        let count = to_leb(types.len());
        size += count.len();

        SectionFunction {
            types: types,
            size: to_leb(size),
            count: count,
        }
    }

    fn encode(&self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(SEC_FUNCTION);
        bytecode.extend(self.size.iter());
        bytecode.extend(self.count.iter());

        // Body
        for t in self.types.iter() {
            bytecode.extend(t);
        }

        bytecode
    }
}

struct SectionCode {
    bodies: Vec<Vec<Instr>>,
    size: Vec<Instr>,
    count: Vec<Instr>,
}

impl SectionCode {
    fn new(funs: &Vec<wasm::Function>) -> SectionCode {
        let mut fun_bodies = Vec::new();
        let mut size = 0;

        for fun in funs {
            let body = &fun.body;

            let mut size_body = to_leb(body.len());
            size_body.extend(body);

            size += size_body.len();
            fun_bodies.push(size_body);
        }

        let count = to_leb(fun_bodies.len());
        size += count.len();

        SectionCode {
            count: count,
            size: to_leb(size),
            bodies: fun_bodies,
        }
    }

    fn encode(&self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(SEC_CODE);
        bytecode.extend(self.size.iter());
        bytecode.extend(self.count.iter());

        // Body
        for body in self.bodies.iter() {
            bytecode.extend(body);
        }

        bytecode
    }
}

struct SectionExport {
    exports: Vec<Vec<Instr>>,
    size: Vec<Instr>,
    count: Vec<Instr>,
}

impl SectionExport {
    fn new(funs: &Vec<wasm::Function>) -> SectionExport {
        let mut exports = Vec::new();
        let mut size = 0;

        for (idx, fun) in funs.iter().enumerate() {
            if let Some(name) = &fun.export_name {
                let mut data = Vec::new();
                let encoded_name = name.as_bytes();

                data.extend(to_leb(encoded_name.len()));
                data.extend(encoded_name);
                data.push(KIND_FUNC);
                data.extend(to_leb(idx));

                size += data.len();
                exports.push(data);
            }
        }

        let count = to_leb(exports.len());
        size += count.len();

        SectionExport {
            exports: exports,
            size: to_leb(size),
            count: count,
        }
    }

    fn encode(&self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(SEC_EXPORT);
        bytecode.extend(self.size.iter());
        bytecode.extend(self.count.iter());

        // Body
        for body in self.exports.iter() {
            bytecode.extend(body);
        }

        bytecode
    }
}

pub struct Module {
    types: SectionType,
    functions: SectionFunction,
    exports: SectionExport,
    code: SectionCode,
}

impl Module {
    pub fn new(mut funs: Vec<wasm::Function>) -> Module {
        let types = SectionType::new(&mut funs); // Must be called first because of side effects
        let functions = SectionFunction::new(&funs);
        let exports = SectionExport::new(&funs);
        let code = SectionCode::new(&funs);
        Module {
            types: types,
            functions: functions,
            code: code,
            exports: exports,
        }
    }

    pub fn encode(&self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.extend(MAGIC_NUMBER.to_le_bytes().iter());
        bytecode.extend(VERSION.to_le_bytes().iter());

        bytecode.extend(self.types.encode().iter());
        bytecode.extend(self.functions.encode().iter());
        bytecode.extend(self.exports.encode());
        bytecode.extend(self.code.encode().iter());

        bytecode
    }
}
