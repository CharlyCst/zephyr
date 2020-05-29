#![allow(dead_code)]

use super::wasm;

pub const MAGIC_NUMBER: u32 = 0x6d736100;
pub const VERSION: u32 = 0x1;

// Section identifier
type SecTyp = u8;
pub const SEC_CUSTOM: SecTyp = 0;
pub const SEC_TYPE: SecTyp = 1;
pub const SEC_IMPORT: SecTyp = 2;
pub const SEC_FUNCTION: SecTyp = 3;
pub const SEC_TABLE: SecTyp = 4;
pub const SEC_MEMORY: SecTyp = 5;
pub const SEC_GLOBAL: SecTyp = 6;
pub const SEC_EXPORT: SecTyp = 7;
pub const SEC_START: SecTyp = 8;
pub const SEC_ELEMENT: SecTyp = 9;
pub const SEC_CODE: SecTyp = 10;
pub const SEC_DATA: SecTyp = 11;

// Kind
type Kind = u8;
pub const KIND_FUNC: Kind = 0;

// Types
type Type = u8;
pub const BLOCK_TYPE: Type = 0x40;
pub const FUNC: Type = 0x60;
pub const ANY_FUNC: Type = 0x70;
pub const I32: Type = 0x7f;
pub const I64: Type = 0x7e;
pub const F32: Type = 0x7d;
pub const F64: Type = 0x7c;

// Instructions
pub type Instr = u8;
// Control
pub const INSTR_UNREACHABLE: Instr = 0x00;
pub const INSTR_NOP: Instr = 0x01;
pub const INSTR_BLOCK: Instr = 0x02;
pub const INSTR_LOOP: Instr = 0x03;
pub const INSTR_IF: Instr = 0x04;
pub const INSTR_ELSE: Instr = 0x05;
pub const INSTR_END: Instr = 0x0b;
pub const INSTR_BR: Instr = 0x0c;
pub const INSTR_BR_IF: Instr = 0x0d;
pub const INSTR_RETURN: Instr = 0x0f;
pub const INSTR_CALL: Instr = 0x10;
pub const INSTR_CALL_INDIRECT: Instr = 0x11;
// Variables
pub const INSTR_LOCAL_GET: Instr = 0x20;
pub const INSTR_LOCAL_SET: Instr = 0x21;
// Numerical
pub const INSTR_I32_CST: Instr = 0x41;
pub const INSTR_I64_CST: Instr = 0x42;
pub const INSTR_F32_CST: Instr = 0x43;
pub const INSTR_F64_CST: Instr = 0x44;
pub const INSTR_I32_EQZ: Instr = 0x45;
pub const INSTR_I32_EQ: Instr = 0x46;
pub const INSTR_I32_NE: Instr = 0x47;
pub const INSTR_I32_LT_S: Instr = 0x48;
pub const INSTR_I32_LT_U: Instr = 0x49;
pub const INSTR_I32_GT_S: Instr = 0x4a;
pub const INSTR_I32_GT_U: Instr = 0x4b;
pub const INSTR_I32_LE_S: Instr = 0x4c;
pub const INSTR_I32_LE_U: Instr = 0x4d;
pub const INSTR_I32_GE_S: Instr = 0x4e;
pub const INSTR_I32_GE_U: Instr = 0x4f;
pub const INSTR_I64_EQZ: Instr = 0x50;
pub const INSTR_I64_EQ: Instr = 0x51;
pub const INSTR_I64_NE: Instr = 0x52;
pub const INSTR_I64_LT_S: Instr = 0x53;
pub const INSTR_I64_LT_U: Instr = 0x54;
pub const INSTR_I64_GT_S: Instr = 0x55;
pub const INSTR_I64_GT_U: Instr = 0x56;
pub const INSTR_I64_LE_S: Instr = 0x57;
pub const INSTR_I64_LE_U: Instr = 0x58;
pub const INSTR_I64_GE_S: Instr = 0x59;
pub const INSTR_I64_GE_U: Instr = 0x5a;
pub const INSTR_I32_ADD: Instr = 0x6a;
pub const INSTR_I32_SUB: Instr = 0x6b;
pub const INSTR_I32_MUL: Instr = 0x6c;
pub const INSTR_I32_DIV_S: Instr = 0x6d;
pub const INSTR_I32_DIV_U: Instr = 0x6e;
pub const INSTR_I32_REM_S: Instr = 0x6f;
pub const INSTR_I32_REM_U: Instr = 0x70;
pub const INSTR_I32_AND: Instr = 0x71;
pub const INSTR_I32_OR: Instr = 0x72;
pub const INSTR_I32_XOR: Instr = 0x73;
pub const INSTR_I64_ADD: Instr = 0x7c;
pub const INSTR_I64_SUB: Instr = 0x7d;
pub const INSTR_I64_MUL: Instr = 0x7e;
pub const INSTR_I64_DIV_S: Instr = 0x7f;
pub const INSTR_I64_DIV_U: Instr = 0x80;
pub const INSTR_I64_REM_S: Instr = 0x81;
pub const INSTR_I64_REM_U: Instr = 0x82;
pub const INSTR_I64_AND: Instr = 0x83;
pub const INSTR_I64_OR: Instr = 0x84;
pub const INSTR_I64_XOR: Instr = 0x85;

const LEB_MASK: usize = 0x0000007f;

pub fn to_leb<'a>(val: usize) -> Vec<u8> {
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

pub fn type_to_bytes(t: wasm::Type) -> u8 {
    match t {
        wasm::Type::F32 => F32,
        wasm::Type::F64 => F64,
        wasm::Type::I32 => I32,
        wasm::Type::I64 => I64,
    }
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
