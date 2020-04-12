use std::collections::HashMap;

use crate::opcode;
use crate::wasm;

pub enum Section {
    Known {
        id: u8,
        payload_len: u32,
        payload_data: Vec<u8>,
    },
    Custom {
        payload_len: u32,
        name: Vec<u8>,
        payload_data: Vec<u8>,
    },
}

struct SectionType {
    types: Vec<Vec<u8>>,
    size: Vec<u8>,
    count: Vec<u8>,
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

            for param in fun.params.iter() {
                params.push(type_to_bytes(&param))
            }

            for result in fun.results.iter() {
                results.push(type_to_bytes(&result))
            }

            fun_type.push(opcode::Func);
            fun_type.append(&mut opcode::to_leb(params.len()));
            fun_type.extend(params);
            fun_type.append(&mut opcode::to_leb(results.len()));
            fun_type.extend(results);

            match known_types.get(&fun_type) {
                Some(idx) => {
                    fun.type_index = *idx;
                }
                None => {
                    size += fun_type.len();
                    known_types.insert(fun_type.clone(), index);

                    types.push(fun_type);
                    fun.type_index = index;
                    index += 1;
                }
            }
        }

        let count = opcode::to_leb(types.len());
        size += count.len();

        SectionType {
            types: types,
            size: opcode::to_leb(size),
            count: count,
        }
    }

    fn encode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(opcode::SEC_TYPE);
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
    types: Vec<Vec<u8>>,
    size: Vec<u8>,
    count: Vec<u8>,
}

impl SectionFunction {
    fn new(funs: &Vec<wasm::Function>) -> SectionFunction {
        let mut types = Vec::new();
        let mut size = 0;

        for fun in funs {
            let idx = opcode::to_leb(fun.type_index);
            size += idx.len();
            types.push(idx);
        }

        let count = opcode::to_leb(types.len());
        size += count.len();

        SectionFunction {
            types: types,
            size: opcode::to_leb(size),
            count: count,
        }
    }

    fn encode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(opcode::SEC_FUNCTION);
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
    bodies: Vec<Vec<u8>>,
    size: Vec<u8>,
    count: Vec<u8>,
}

impl SectionCode {
    fn new(funs: &Vec<wasm::Function>) -> SectionCode {
        let mut fun_bodies = Vec::new();
        let mut size = 0;

        for fun in funs {
            let body = &fun.body;

            let mut size_body = opcode::to_leb(body.len());
            size_body.extend(body);

            size += size_body.len();
            fun_bodies.push(size_body);
        }

        let count = opcode::to_leb(fun_bodies.len());
        size += count.len();

        SectionCode {
            count: count,
            size: opcode::to_leb(size),
            bodies: fun_bodies,
        }
    }

    fn encode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(opcode::SEC_CODE);
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
    exports: Vec<Vec<u8>>,
    size: Vec<u8>,
    count: Vec<u8>,
}

impl SectionExport {
    fn new(funs: &Vec<wasm::Function>) -> SectionExport {
        let mut exports = Vec::new();
        let mut size = 0;

        for (idx, fun) in funs.iter().enumerate() {
            if let Some(name) = &fun.export_name {
                let mut data = Vec::new();
                let encoded_name = name.as_bytes();

                data.extend(opcode::to_leb(encoded_name.len()));
                data.extend(encoded_name);
                data.push(opcode::KIND_FUNC);
                data.extend(opcode::to_leb(idx));

                size += data.len();
                exports.push(data);
            }
        }

        let count = opcode::to_leb(exports.len());
        size += count.len();

        SectionExport {
            exports: exports,
            size: opcode::to_leb(size),
            count: count,
        }
    }

    fn encode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(opcode::SEC_EXPORT);
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

    pub fn encode(&self) -> Vec<u8> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.extend(opcode::MAGIC_NUMBER.to_le_bytes().iter());
        bytecode.extend(opcode::VERSION.to_le_bytes().iter());

        bytecode.extend(self.types.encode().iter());
        bytecode.extend(self.functions.encode().iter());
        bytecode.extend(self.exports.encode());
        bytecode.extend(self.code.encode().iter());

        bytecode
    }
}

fn type_to_bytes(t: &wasm::Type) -> u8 {
    match t {
        wasm::Type::F32 => opcode::F32,
        wasm::Type::F64 => opcode::F64,
        wasm::Type::I32 => opcode::I32,
        wasm::Type::I64 => opcode::I64,
    }
}
