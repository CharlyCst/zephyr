pub type Opcode = u8;
pub const END: Opcode = 0x0b;

// Section identifier
pub type SecTyp = u8;
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
