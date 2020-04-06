use std::env;
use std::fs;

mod error;
mod parse;
mod scan;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: fork <file>");
    } else {
        let code = fs::read_to_string(&args[1]).expect("File not found");
        compile(code);
    }
}

fn compile(code: String) {
    println!("\n/// Scanning ///\n");

    let scan_handler = error::ErrorHandler::new();
    let mut scanner = scan::Scanner::new(scan_handler, code);
    let tokens = scanner.scan();

    for token in tokens.iter() {
        print!("{}", token);
    }
    println!("");

    println!("\n/// Parsing ///\n");

    let parse_handler = error::ErrorHandler::new();
    let mut parser = parse::Parser::new(parse_handler, tokens);
    let statements = parser.parse();

    for stmt in statements.iter() {
        println!("{}", stmt);
    }

    if parser.success() {
        println!("\nSuccess");
    } else {
        println!("\nFailure");
    }
    return;
}
