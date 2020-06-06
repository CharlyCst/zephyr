use crate::error::ErrorHandler;

mod fasm;
mod scan;
mod tokens;

pub fn get_ast<'a, 'b>(code: &str, error_handler: &'b mut ErrorHandler<'a>) {
    println!("\n/// Scanning ///\n");

    let mut scanner = scan::Scanner::new(code, 0, error_handler);
    let tokens = scanner.scan();

    for token in tokens.iter() {
        print!("{} ", token);
    }
    println!("");
}
