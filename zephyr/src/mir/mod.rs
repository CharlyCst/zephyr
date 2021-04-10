use crate::ctx::{Ctx, KnownFunctions};
use crate::error::ErrorHandler;

pub use mir::*;

mod hir_to_mir;
mod mir;

pub use mir::Program;

pub fn to_mir(
    ctx: &Ctx,
    known_funs: &KnownFunctions,
    error_handler: &mut impl ErrorHandler,
    verbose: bool,
) -> mir::Program {
    if verbose {
        println!("\n/// MIR Production ///\n");
    }

    let mir = hir_to_mir::MirProducer::lower(ctx, known_funs, error_handler);

    if verbose {
        println!("{}", mir);
    }

    error_handler.flush_and_exit_if_err();
    mir
}
