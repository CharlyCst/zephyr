use super::opcode;
use super::opcode::to_leb;

pub type Offset = u32;

pub const PAGE_SIZE: u32 = 0xffff;

pub struct Function {
    pub param_types: Vec<Type>,
    pub ret_types: Vec<Type>,
    pub type_idx: usize, // Used by encode
    pub exposed: Option<String>,
    pub fun_id: u64,
    pub body: Vec<opcode::Instr>,
}

pub struct Import {
    pub module: String,
    pub name: String,
    pub param_types: Vec<Type>,
    pub ret_types: Vec<Type>,
    pub kind: opcode::Kind,
    pub type_idx: usize, // Used by encode
}

#[derive(Copy, Clone)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}

/// Describe a range.
/// Used to specify the initial/maximal size of a memory in pages (64Ki).
#[allow(dead_code)] // MinMax never used for now.
pub enum Limit {
    Min(u32),
    MinMax(u32, u32),
}

/// A struct representing a Wasm vector (as specified by the binary format).
/// It implements IntoIterator and can be directly passed where a wasm `vec`
/// is expected.
pub struct WasmVec {
    vec: Vec<u8>,
    size: u64,
}

impl WasmVec {
    /// Returns a fresh WasmVec.
    pub fn new() -> Self {
        WasmVec {
            vec: Vec::new(),
            size: 0,
        }
    }

    /// Extend the WasmVec with an iterator of bytes representing one item.
    pub fn extend_item<T: IntoIterator<Item = u8>>(&mut self, iter: T) {
        self.vec.extend(iter);
        self.size += 1;
    }

    /// Push a single byte item to the end of the WasmVec.
    pub fn push_item(&mut self, byte: u8) {
        self.vec.push(byte);
        self.size += 1;
    }

    /// Return the size (in bytes) of this vector
    pub fn size(&self) -> u64 {
        let header_size = to_leb(self.size).len(); // TODO: don't need to build the vector to get its len.
        (self.vec.len() + header_size) as u64
    }
}

impl std::iter::IntoIterator for WasmVec {
    type Item = u8;
    type IntoIter = std::iter::Chain<std::vec::IntoIter<u8>, std::vec::IntoIter<u8>>;

    fn into_iter(self) -> Self::IntoIter {
        let content_iter = self.vec.into_iter();
        to_leb(self.size).into_iter().chain(content_iter)
    }
}

impl From<Vec<u8>> for WasmVec {
    /// Build a WasmVec from a raw buffer of data.
    /// Each byte is counted as one element.
    fn from(data: Vec<u8>) -> Self {
        Self {
            size: data.len() as u64,
            vec: data,
        }
    }
}

pub struct DataSegment {
    mem_idx: u32,
    offset: u32,
    data: WasmVec,
}

impl DataSegment {
    pub fn new(mem_idx: u32, offset: u32, data: WasmVec) -> Self {
        Self {
            mem_idx,
            offset,
            data,
        }
    }

    pub fn encode(self) -> Vec<u8> {
        let mut data_segment = Vec::new();
        // mem_idx
        data_segment.extend(to_leb(self.mem_idx as u64));
        // offset
        data_segment.push(opcode::INSTR_I32_CST);
        data_segment.extend(to_leb(self.offset as u64));
        data_segment.push(opcode::INSTR_END);
        // data
        data_segment.extend(self.data);

        data_segment
    }
}

impl std::iter::IntoIterator for DataSegment {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.encode().into_iter()
    }
}
