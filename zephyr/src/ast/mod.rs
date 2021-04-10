use crate::ctx::ModId;
use crate::error::ErrorHandler;
use crate::resolver::{FileId, FileKind};

mod asm_parse;
mod asm_scan;
mod asm_tokens;
mod ast;
mod opcode_to_asm;
mod parse;
mod scan;
mod tokens;

pub use ast::*;
pub use tokens::*;

/// Returns the file AST.
/// The file content corresponding to `f_id` must be owned by the error_handler.
pub fn get_ast(
    f_id: FileId,
    mod_id: ModId,
    kind: FileKind,
    error_handler: &mut impl ErrorHandler,
    verbose: bool,
) -> ast::Program {
    match kind {
        FileKind::Zephyr => get_zephyr_ast(f_id, mod_id, error_handler, verbose),
        FileKind::Asm => get_asm_ast(f_id, mod_id, error_handler, verbose),
    }
}

fn get_zephyr_ast(
    f_id: FileId,
    mod_id: ModId,
    error_handler: &mut impl ErrorHandler,
    verbose: bool,
) -> ast::Program {
    if verbose {
        println!("\n/// Scanning ///\n");
    }

    let mut scanner = scan::Scanner::new(f_id, error_handler);
    let tokens = scanner.scan();

    if verbose {
        for token in tokens.iter() {
            print!("{}", token);
        }
        println!("");
        println!("\n/// Parsing ///\n");
    }

    let mut parser = parse::Parser::new(tokens, mod_id, error_handler);
    let ast_program = parser.parse();

    if verbose {
        println!("{}", ast_program);
    }

    error_handler.flush_and_exit_if_err();
    ast_program
}

fn get_asm_ast(
    f_id: FileId,
    mod_id: ModId,
    error_handler: &mut impl ErrorHandler,
    verbose: bool,
) -> ast::Program {
    if verbose {
        println!("\n/// Scanning ///\n");
    }

    let mut scanner = asm_scan::Scanner::new(f_id, error_handler);
    let tokens = scanner.scan();

    if verbose {
        for token in tokens.iter() {
            print!("{} ", token);
        }
        println!("\n");
        println!("\n/// Parsing ///\n");
    }

    error_handler.flush_and_exit_if_err();

    let mut parser = asm_parse::Parser::new(tokens, mod_id, error_handler);
    let ast_program = parser.parse();

    if verbose {
        println!("{}", ast_program);
    }

    error_handler.flush_and_exit_if_err();
    ast_program
}
