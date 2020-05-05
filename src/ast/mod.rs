use crate::error::ErrorHandler;

mod ast;
mod parse;
mod scan;
mod tokens;

pub use ast::*;
pub use tokens::*;

pub fn get_ast(code: String, error_handler: &mut ErrorHandler) -> ast::Program {
    println!("\n/// Scanning ///\n");

    let mut scanner = scan::Scanner::new(code, error_handler);
    let tokens = scanner.scan();

    for token in tokens.iter() {
        print!("{}", token);
    }
    println!("");

    println!("\n/// Parsing ///\n");

    let mut parser = parse::Parser::new(tokens, error_handler);
    let ast_program = parser.parse();
    println!("{}", ast_program);
    ast_program
}
