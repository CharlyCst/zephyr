use std::collections::HashMap;

use crate::opcode;
use crate::wasm;

const LEB_MASK: usize = 0x0000007f;

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
            fun_type.append(&mut to_leb(params.len()));
            fun_type.extend(params);
            fun_type.append(&mut to_leb(results.len()));
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

        let count = to_leb(types.len());
        size += count.len();

        SectionType {
            types: types,
            size: to_leb(size),
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
            let idx = to_leb(fun.type_index);
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
            let mut body = Vec::new();

            body.push(0x00); // local count
            body.push(opcode::INSTR_END);

            let mut size_body = to_leb(body.len());
            size_body.append(&mut body);

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

                data.extend(to_leb(encoded_name.len()));
                data.extend(encoded_name);
                data.push(opcode::KIND_FUNC);
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

fn to_leb<'a>(val: usize) -> Vec<u8> {
    let mut remainder = val;
    let mut leb = Vec::new();
    let mut done = false;
    while !done {
        let mut byte = (LEB_MASK & remainder) as u8;
        remainder = remainder >> 7;
        if remainder == 0 {
            done = true;
        } else {
            byte += 0x80;
        }
        leb.push(byte);
    }
    leb
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_leb() {
        assert_eq!(vec!(0x0), to_leb(0));
        assert_eq!(vec!(0x5), to_leb(5));
        assert_eq!(vec!(0x80, 0x1), to_leb(128));
        assert_eq!(vec!(0xff, 0x1), to_leb(255));
    }
}
