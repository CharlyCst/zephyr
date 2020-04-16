use std::env;
use std::fs;

mod compile;
mod encode;
mod error;
mod name;
mod opcode;
mod parse;
mod scan;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut output_path = "a.wasm";
    if args.len() == 3 {
        output_path = &args[2];
    } else if args.len() != 2 {
        println!("Usage: fork <file> [out]");
        std::process::exit(1);
    }
    let code = fs::read_to_string(&args[1]).expect("File not found");
    compile(code, output_path);
}

fn compile(code: String, output_path: &str) {
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
        std::process::exit(1);
    }

    let mut name_resolver = name::NameResolver::new();
    let program = name_resolver.resolve(functions);

    println!("\n/// Name Resolution ///\n");

    println!("{}\n", program.names);
    println!("{}", program.types);

    // let mut compiler = compile::Compiler::new();
    // let wasm_functions = compiler.compile(functions);

    // let module = encode::Module::new(wasm_functions);
    // match fs::write(output_path, module.encode()) {
    //     Ok(_) => (),
    //     Err(e) => println!("{}", e),
    // }
    std::process::exit(0);
}
