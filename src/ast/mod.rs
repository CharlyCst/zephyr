use crate::error::ErrorHandler;

mod ast;
mod parse;
mod scan;
mod tokens;

pub use ast::*;
pub use tokens::*;

pub fn get_ast(f_id: u16, error_handler: &mut ErrorHandler) -> ast::Program {
    println!("\n/// Scanning ///\n");

    let mut scanner = scan::Scanner::new(f_id, error_handler);
    let tokens = scanner.scan();

    for token in tokens.iter() {
        print!("{}", token);
    }
    println!("");

    println!("\n/// Parsing ///\n");

    let mut parser = parse::Parser::new(tokens, error_handler);
    let ast_program = parser.parse();
    println!("{}", ast_program);

    error_handler.flush_and_exit_if_err();

    ast_program
}
