use crate::cli::Config;
use crate::error::ErrorHandler;

mod ast;
mod parse;
mod scan;
mod tokens;

pub use ast::*;
pub use tokens::*;

pub fn get_ast(
    f_id: u16,
    package_id: u32,
    error_handler: &mut ErrorHandler,
    config: &Config,
) -> ast::Program {
    if config.verbose {
        println!("\n/// Scanning ///\n");
    }

    let mut scanner = scan::Scanner::new(f_id, error_handler);
    let tokens = scanner.scan();

    if config.verbose {
        for token in tokens.iter() {
            print!("{}", token);
        }
        println!("");
        println!("\n/// Parsing ///\n");
    }

    let mut parser = parse::Parser::new(tokens, package_id, error_handler);
    let ast_program = parser.parse();

    if config.verbose {
        println!("{}", ast_program);
    }

    error_handler.flush_and_exit_if_err();
    ast_program
}
