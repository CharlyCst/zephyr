use std::env;
use std::fs;

mod error;
mod scan;

fn main() {
    println!("Hi, I'm the Fork compiler!");

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: fork <file>");
    } else {
        let code = fs::read_to_string(&args[1]).expect("File not found");
        compile(code);
    }
}

fn compile(code: String) {
    let error_handler = error::ErrorHandler::new();
    let mut scanner = scan::Scanner::new(error_handler, code);
    let tokens = scanner.scan();
    println!("{:?}", tokens);
    return;
}
