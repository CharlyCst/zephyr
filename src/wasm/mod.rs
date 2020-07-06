use crate::cli::Config;
use crate::error::ErrorHandler;
use crate::mir;

mod mir_to_wasm;
mod opcode;
mod sections;
mod wasm;

pub fn to_wasm<'a>(
    mir_program: mir::Program,
    error_handler: &'a mut ErrorHandler,
    config: &Config,
) -> Vec<u8> {
    if config.verbose {
        println!("\n/// Compiling ///\n");
    }

    let mut compiler = mir_to_wasm::Compiler::new(error_handler);
    let program = compiler.compile(mir_program);

    error_handler.flush_and_exit_if_err();

    program
}
