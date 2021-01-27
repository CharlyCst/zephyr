use crate::error::ErrorHandler;

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

#[derive(Debug)]
pub enum Kind {
    Zephyr,
    Asm,
}

/// Returns the file AST.
/// The file content corresponding to `f_id` must be owned by the error_handler.
pub fn get_ast(
    f_id: u16,
    package_id: u32,
    kind: Kind,
    error_handler: &mut ErrorHandler,
    verbose: bool,
) -> ast::Program {
    match kind {
        Kind::Zephyr => get_zephyr_ast(f_id, package_id, error_handler, verbose),
        Kind::Asm => get_asm_ast(f_id, package_id, error_handler, verbose),
    }
}

fn get_zephyr_ast(
    f_id: u16,
    package_id: u32,
    error_handler: &mut ErrorHandler,
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

    let mut parser = parse::Parser::new(tokens, package_id, error_handler);
    let ast_program = parser.parse();

    if verbose {
        println!("{}", ast_program);
    }

    error_handler.flush_and_exit_if_err();
    ast_program
}

fn get_asm_ast(
    f_id: u16,
    package_id: u32,
    error_handler: &mut ErrorHandler,
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

    let mut parser = asm_parse::Parser::new(tokens, package_id, error_handler);
    let ast_program = parser.parse();

    if verbose {
        println!("{}", ast_program);
    }

    error_handler.flush_and_exit_if_err();
    ast_program
}
