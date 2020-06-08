use crate::error::ErrorHandler;

mod fasm;
mod opcode_to_mir;
mod parse;
mod scan;
mod tokens;

pub fn get_ast<'a, 'b>(code: &str, error_handler: &'b mut ErrorHandler<'a>) {
    println!("\n/// Scanning ///\n");

    let mut scanner = scan::Scanner::new(code, 0, error_handler);
    let tokens = scanner.scan();

    for token in tokens.iter() {
        print!("{} ", token);
    }
    println!("\n");
    error_handler.print_and_exit();

    println!("\n/// Parsing ///\n");

    let mut parser = parse::Parser::new(tokens, error_handler);
    let program = parser.parse();
    println!("{}", program);

    error_handler.print_and_exit();
}
