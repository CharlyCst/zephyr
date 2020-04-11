use std::env;
use std::fs;

mod encode;
mod error;
mod opcode;
mod parse;
mod scan;
mod wasm;

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

    let mut scanner = scan::Scanner::new(code);
    let tokens = scanner.scan();

    for token in tokens.iter() {
        print!("{}", token);
    }
    println!("");

    println!("\n/// Parsing ///\n");

    let mut parser = parse::Parser::new(tokens);
    let functions = parser.parse();

    for stmt in functions.iter() {
        println!("{}", stmt);
    }

    if parser.success() {
        println!("\nSuccess");
    } else {
        println!("\nFailure");
    }

    let mut compiler = wasm::Compiler::new();
    let wasm_functions = compiler.compile(functions);

    let module = encode::Module::new(wasm_functions);
    match fs::write("out/hello.wasm", module.encode()) {
        Ok(_) => (),
        Err(e) => println!("{}", e),
    }

    return;
}
