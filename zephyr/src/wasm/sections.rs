#[warn()]
use std::collections::HashMap;

use super::opcode::*;
use super::wasm;
use super::wasm::{DataSegment, Offset, WasmVec};

/// Handles type index attribution and storage.
struct TypeStore {
    index: usize,
    existing_types: HashMap<Vec<u8>, usize>,
    types: WasmVec,
}

impl TypeStore {
    fn new() -> Self {
        Self {
            index: 0,
            existing_types: HashMap::new(),
            types: WasmVec::new(),
        }
    }

    /// Consumes a type to return its index.
    fn get_idx(&mut self, t: Vec<u8>) -> usize {
        match self.existing_types.get(&t) {
            Some(idx) => *idx,
            None => {
                let idx = self.index;
                self.existing_types.insert(t.clone(), idx);
                self.types.extend_item(t);
                self.index += 1;
                idx
            }
        }
    }

    /// Returns a WasmVec of types.
    fn get_types(self) -> WasmVec {
        self.types
    }
}

struct SectionType {
    types: WasmVec,
}

impl SectionType {
    // Function declaration format:
    // [Func] (nb_args) [arg_1] [arg_2] ... (nb_results) [result_1] [result_2] ...
    fn new(funs: &mut Vec<wasm::Function>, imports: &mut Vec<wasm::Import>) -> Self {
        let mut type_store = TypeStore::new();
        for fun in funs.iter_mut() {
            let fun_type = SectionType::build_type(&fun.param_types, &fun.ret_types);
            fun.type_idx = type_store.get_idx(fun_type);
        }
        for import in imports.iter_mut() {
            let fun_type = SectionType::build_type(&import.param_types, &import.ret_types);
            import.type_idx = type_store.get_idx(fun_type);
        }

        Self {
            types: type_store.get_types(),
        }
    }

    /// Builds a function type from parameters and return types.
    fn build_type(param_types: &Vec<wasm::Type>, ret_types: &Vec<wasm::Type>) -> Vec<u8> {
        let mut params = WasmVec::new();
        let mut results = WasmVec::new();
        let mut fun_type = Vec::new();

        for t in param_types.iter() {
            params.push_item(type_to_bytes(*t))
        }

        for t in ret_types.iter() {
            results.push_item(type_to_bytes(*t))
        }

        fun_type.push(FUNC);
        fun_type.extend(params);
        fun_type.extend(results);
        fun_type
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_TYPE);
        bytecode.extend(to_leb(self.types.size()));
        bytecode.extend(self.types);

        bytecode
    }
}

struct SectionImport {
    imports: WasmVec,
}

impl SectionImport {
    fn new(imports: Vec<wasm::Import>) -> Self {
        let mut wasm_imports = WasmVec::new();
        for import in imports {
            let mut raw_import = Vec::new();
            let mut module = WasmVec::new();
            let mut name = WasmVec::new();
            for byte in import.module.as_bytes() {
                module.push_item(*byte);
            }
            for byte in import.name.as_bytes() {
                name.push_item(*byte);
            }
            raw_import.extend(module);
            raw_import.extend(name);
            raw_import.push(import.kind);
            raw_import.extend(to_leb(import.type_idx as u64));
            wasm_imports.extend_item(raw_import);
        }
        Self {
            imports: wasm_imports,
        }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_IMPORT);
        bytecode.extend(to_leb(self.imports.size()));
        bytecode.extend(self.imports);

        bytecode
    }
}

struct SectionFunction {
    types: WasmVec,
}

impl SectionFunction {
    fn new(funs: &Vec<wasm::Function>) -> Self {
        let mut types = WasmVec::new();

        // The function index corresponds to its index in funs
        for fun in funs {
            let idx = to_leb(fun.type_idx as u64);
            types.extend_item(idx);
        }

        Self { types }
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
    fn new(memories: Vec<wasm::Limit>) -> Self {
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

        Self { memories: mems }
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
    fn new(funs: &Vec<wasm::Function>) -> Self {
        let mut fun_bodies = WasmVec::new();

        for fun in funs {
            let body = &fun.body;
            let mut sized_body = to_leb(body.len() as u64);
            sized_body.extend(body);
            fun_bodies.extend_item(sized_body);
        }

        Self { bodies: fun_bodies }
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
    fn new(funs: &Vec<wasm::Function>) -> Self {
        let mut exports = WasmVec::new();

        // Export functions
        for fun in funs.iter() {
            if let Some(name) = &fun.exposed {
                let mut data = Vec::new();
                let encoded_name = name.as_bytes();

                data.extend(to_leb(encoded_name.len() as u64));
                data.extend(encoded_name);
                data.push(KIND_FUNC);
                data.extend(to_leb(fun.fun_id));

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

        Self { exports }
    }

    fn encode(self) -> Vec<Instr> {
        let mut bytecode = Vec::new();

        bytecode.push(SEC_EXPORT);
        bytecode.extend(to_leb(self.exports.size()));
        bytecode.extend(self.exports);

        bytecode
    }
}

pub struct SectionData {
    data: WasmVec,
    offset: Offset,
    nb_pages: u32,
}

impl SectionData {
    pub fn new() -> Self {
        // Offset is initialized to 8 as the first bytes are reserved by the allocator.
        Self { data: WasmVec::new(), offset: 8, nb_pages: 1 }
    }

    /// Insert a new data segment and return its offset.
    pub fn add_data_segment(&mut self, data: Vec<u8>) -> Offset {
        let offset = self.offset;
        let len = data.len() as Offset;
        self.data
            .extend_item(DataSegment::new(0, offset, data.into()));

        // Maintain an offset such that an aligment of 8 is always guaranteed.
        if len % 8 != 0 {
            self.offset = offset + len + 8 - (len % 8);
        } else {
            self.offset = offset + len;
        }
        offset
    }

    /// Insert memory segments needed to initialize the memory allocator.
    ///
    /// ! Caution: this function assumes that no other data segment will be added, call it just
    /// before encoding the SectionData into raw wasm.
    fn add_allocator_segments(&mut self) {
        let mut first_block_header = Vec::new();

        // The memory allocator expects to find the address of the first block
        // encoded as a u32 at mem[0]. This address should be carefully picked
        // so that data will be aligned (see allocator).

        let offset = self.offset; // Aligned to 8 bytes
        let first_block_offset = offset + 4; // Offset of the first block header

        // mem[0..4] - address of first block
        self.data.extend_item(DataSegment::new(
            0,
            0,
            first_block_offset.to_le_bytes().to_vec().into(),
        ));

        // mem[offset..(offset + 4)] - mocked block footer with allocated bit set
        let footer: u32 = 0xffffffff;
        first_block_header.extend(&footer.to_le_bytes());
        // mem[(offset + 4)..(offset + 8)] - first block header (its size)
        let block_size = wasm::PAGE_SIZE * self.nb_pages - (first_block_offset + 4);
        first_block_header.extend(&block_size.to_le_bytes());

        self.data
            .extend_item(DataSegment::new(0, offset, first_block_header.into()));
    }

    fn encode(mut self) -> Vec<Instr> {
        self.add_allocator_segments();
        let mut bytecode = Vec::new();

        bytecode.push(SEC_DATA);
        bytecode.extend(to_leb(self.data.size()));
        bytecode.extend(self.data);

        bytecode
    }
}

pub struct Module {
    types: SectionType,
    imports: SectionImport,
    functions: SectionFunction,
    memories: SectionMemory,
    exports: SectionExport,
    code: SectionCode,
    data: SectionData,
}

impl Module {
    pub fn new(mut funs: Vec<wasm::Function>, mut imports: Vec<wasm::Import>, data: SectionData) -> Self {
        let types = SectionType::new(&mut funs, &mut imports); // Must be called first because of side effects
        let imports = SectionImport::new(imports);
        let functions = SectionFunction::new(&funs);
        let memories = SectionMemory::new(vec![wasm::Limit::Min(1)]);
        let exports = SectionExport::new(&funs);
        let code = SectionCode::new(&funs);
        Self {
            types,
            imports,
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

        // Sections
        bytecode.extend(self.types.encode());
        bytecode.extend(self.imports.encode());
        bytecode.extend(self.functions.encode());
        bytecode.extend(self.memories.encode());
        bytecode.extend(self.exports.encode());
        bytecode.extend(self.code.encode());
        bytecode.extend(self.data.encode());

        bytecode
    }
}
