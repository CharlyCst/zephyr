use crate::error::ErrorHandler;
use crate::mir;

mod mir_to_wasm;
mod opcode;
mod sections;
mod wasm;

pub fn to_wasm<'a, 'b>(
    mir_program: mir::Program,
    error_handler: &'b mut ErrorHandler<'a>,
) -> Vec<u8> {
    println!("\n/// Compiling ///\n");

    let mut compiler = mir_to_wasm::Compiler::new(error_handler);
    let program = compiler.compile(mir_program);

    error_handler.print_and_exit();

    program
}
