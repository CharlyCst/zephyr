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
pub const I32: Type = 0x7f;
pub const I64: Type = 0x7e;
pub const F32: Type = 0x7d;
pub const F64: Type = 0x7c;
pub const AnyFunc: Type = 0x70;
pub const Func: Type = 0x60;
pub const BlockType: Type = 0x40;

// Instructions
pub type Instr = u8;
pub const INSTR_UNREACHABLE: Instr = 0x00;
pub const INSTR_NOP: Instr = 0x01;
pub const INSTR_END: Instr = 0x0b;
pub const INSTR_RETURN: Instr = 0x0f;
pub const INSTR_I32_CST: Instr = 0x41;
pub const INSTR_I64_CST: Instr = 0x42;
pub const INSTR_F32_CST: Instr = 0x43;
pub const INSTR_F64_CST: Instr = 0x44;

pub const INSTR_I32_ADD: Instr = 0x6a;

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
