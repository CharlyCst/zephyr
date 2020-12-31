use crate::cli::Config;
use crate::error::ErrorHandler;
use crate::hir::Program as HirProgram;

pub use mir::*;

mod hir_to_mir;
mod known_functions;
mod mir;

pub use known_functions::KnownFunctions;
pub use mir::Program;

pub fn to_mir<'a>(
    hir_program: HirProgram,
    known_funs: known_functions::KnownFunctions,
    error_handler: &mut ErrorHandler,
    config: &Config,
) -> mir::Program {
    if config.verbose {
        println!("\n/// MIR Production ///\n");
    }

    let mut mir_producer = hir_to_mir::MIRProducer::new(error_handler);
    let mir = mir_producer.reduce(hir_program, known_funs);

    if config.verbose {
        println!("{}", mir);
    }

    error_handler.flush_and_exit_if_err();
    mir
}
