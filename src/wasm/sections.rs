#[warn()]
use std::collections::HashMap;

use super::opcode::*;
use super::wasm;
use super::wasm::WasmVec;

struct SectionType {
    types: WasmVec,
}

impl SectionType {
    // Function declaration format:
    // [Func] (nb_args) [arg_1] [arg_2] ... (nb_results) [result_1] [result_2] ...
    fn new(funs: &mut Vec<wasm::Function>) -> SectionType {
        let mut types = WasmVec::new();
        let mut index: usize = 0;
        let mut known_types = HashMap::new();

        for fun in funs.iter_mut() {
            let mut params = WasmVec::new();
            let mut results = WasmVec::new();
            let mut fun_type = Vec::new();

            for t in fun.param_types.iter() {
                params.push_item(type_to_bytes(*t))
            }

            for t in fun.ret_types.iter() {
                results.push_item(type_to_bytes(*t))
            }

            fun_type.push(FUNC);
            fun_type.extend(params);
            fun_type.extend(results);

            match known_types.get(&fun_type) {
                Some(idx) => {
                    fun.type_idx = *idx;
                }
                None => {
                    known_types.insert(fun_type.clone(), index);

                    types.extend_item(fun_type);
                    fun.type_idx = index;
                    index += 1;
                }
            }
        }

        SectionType { types }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_TYPE);
        bytecode.extend(to_leb(self.types.size()));
        bytecode.extend(self.types);

        bytecode
    }
}

struct SectionFunction {
    types: WasmVec,
}

impl SectionFunction {
    fn new(funs: &Vec<wasm::Function>) -> SectionFunction {
        let mut types = WasmVec::new();

        // The function index corresponds to its index in funs
        for fun in funs {
            let idx = to_leb(fun.type_idx as u64);
            types.extend_item(idx);
        }

        SectionFunction { types }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_FUNCTION);
        bytecode.extend(to_leb(self.types.size()));
        bytecode.extend(self.types);

        bytecode
    }
}

struct SectionMemory {
    memories: WasmVec,
}

impl SectionMemory {
    fn new(memories: Vec<wasm::Limit>) -> SectionMemory {
        let mut mems = WasmVec::new();

        for memory in memories {
            let mut mem = Vec::new();
            match memory {
                wasm::Limit::Min(min) => {
                    mem.push(0x00); // No upper limit flag
                    mem.extend(to_leb(min as u64));
                }
                wasm::Limit::MinMax(min, max) => {
                    mem.push(0x01); // With upper limit flag
                    mem.extend(to_leb(min as u64));
                    mem.extend(to_leb(max as u64));
                }
            }
            mems.extend_item(mem);
        }

        SectionMemory { memories: mems }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.push(SEC_MEMORY);
        bytecode.extend(to_leb(self.memories.size()));
        bytecode.extend(self.memories);

        bytecode
    }
}

struct SectionCode {
    bodies: WasmVec,
}

impl SectionCode {
    fn new(funs: &Vec<wasm::Function>) -> SectionCode {
        let mut fun_bodies = WasmVec::new();

        for fun in funs {
            let body = &fun.body;
            let mut sized_body = to_leb(body.len() as u64);
            sized_body.extend(body);
            fun_bodies.extend_item(sized_body);
        }

        SectionCode { bodies: fun_bodies }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_CODE);
        bytecode.extend(to_leb(self.bodies.size()));
        bytecode.extend(self.bodies);

        bytecode
    }
}

struct SectionExport {
    exports: WasmVec,
}

impl SectionExport {
    fn new(funs: &Vec<wasm::Function>) -> SectionExport {
        let mut exports = WasmVec::new();

        // Export functions
        for (idx, fun) in funs.iter().enumerate() {
            if let Some(name) = &fun.exposed {
                let mut data = Vec::new();
                let encoded_name = name.as_bytes();

                data.extend(to_leb(encoded_name.len() as u64));
                data.extend(encoded_name);
                data.push(KIND_FUNC);
                data.extend(to_leb(idx as u64));

                exports.extend_item(data);
            }
        }

        // Export memory
        let mem_name = "memory".as_bytes();
        let mut data = Vec::new();
        data.extend(to_leb(mem_name.len() as u64));
        data.extend(mem_name);
        data.push(KIND_MEM);
        data.push(0);
        exports.extend_item(data);

        SectionExport { exports }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_EXPORT);
        bytecode.extend(to_leb(self.exports.size()));
        bytecode.extend(self.exports);

        bytecode
    }
}

struct SectionData {
    data: WasmVec,
}

impl SectionData {
    fn new() -> Self {
        let mut data = WasmVec::new();

        // The memory allocator expects to find the address of the first block
        // encoded as a u32 at mem[0]. This address should be carefully picked
        // so that data will be aligned (see allocator).
        let mut hardcoded_data = WasmVec::new();
        // mem[0..4]
        hardcoded_data.push_item(4);
        hardcoded_data.push_item(0);
        hardcoded_data.push_item(0);
        hardcoded_data.push_item(0);
        // mem[4..8]
        hardcoded_data.push_item(0);
        hardcoded_data.push_item(0);
        hardcoded_data.push_item(1);
        hardcoded_data.push_item(0);
        // mem_idx:0, offset: 0
        let mut data_segment = Vec::new();
        // mem_idx: 0
        data_segment.extend(to_leb(0));
        // offset: 0
        data_segment.push(INSTR_I32_CST);
        data_segment.extend(to_leb(0));
        data_segment.push(INSTR_END);
        // vector of bytes: 32
        data_segment.extend(hardcoded_data);

        data.extend_item(data_segment);
        SectionData { data }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_DATA);
        bytecode.extend(to_leb(self.data.size()));
        bytecode.extend(self.data);

        bytecode
    }
}

pub struct Module {
    types: SectionType,
    functions: SectionFunction,
    memories: SectionMemory,
    exports: SectionExport,
    code: SectionCode,
    data: SectionData,
}

impl Module {
    pub fn new(mut funs: Vec<wasm::Function>) -> Module {
        let types = SectionType::new(&mut funs); // Must be called first because of side effects
        let functions = SectionFunction::new(&funs);
        let memories = SectionMemory::new(vec![wasm::Limit::Min(1)]);
        let exports = SectionExport::new(&funs);
        let code = SectionCode::new(&funs);
        let data = SectionData::new();
        Module {
            types,
            functions,
            memories,
            code,
            exports,
            data,
        }
    }

    pub fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        // Header
        bytecode.extend(MAGIC_NUMBER.to_le_bytes().iter());
        bytecode.extend(VERSION.to_le_bytes().iter());

        bytecode.extend(self.types.encode());
        bytecode.extend(self.functions.encode());
        bytecode.extend(self.memories.encode());
        bytecode.extend(self.exports.encode());
        bytecode.extend(self.code.encode());
        bytecode.extend(self.data.encode());

        bytecode
    }
}
