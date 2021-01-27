use crate::driver::{Ctx, KnownFunctions};
use crate::error::ErrorHandler;

pub use mir::*;

mod hir_to_mir;
mod mir;

pub use mir::Program;

pub fn to_mir(
    ctx: &Ctx,
    known_funs: &KnownFunctions,
    error_handler: &mut ErrorHandler,
    verbose: bool,
) -> mir::Program {
    if verbose {
        println!("\n/// MIR Production ///\n");
    }

    let mut mir_producer = hir_to_mir::MIRProducer::new(error_handler);
    let mir = mir_producer.reduce(ctx, known_funs);

    if verbose {
        println!("{}", mir);
    }

    error_handler.flush_and_exit_if_err();
    mir
}
