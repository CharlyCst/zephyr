use crate::mir;

mod mir_to_wasm;
mod opcode;
mod sections;
mod wasm;

pub fn to_wasm(mir_program: mir::Program) -> Vec<u8> {
    println!("\n/// Compiling ///\n");

    let mut compiler = mir_to_wasm::Compiler::new();
    compiler.compile(mir_program)
}
