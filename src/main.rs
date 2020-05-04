use std::env;
use std::fs;

mod ast;
mod error;
mod mir;
mod wasm;

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
    let ast_program = ast::get_ast(code);
    let mir_program = mir::to_mir(ast_program);
    let binary = wasm::to_wasm(mir_program);

    match fs::write(output_path, binary) {
        Ok(_) => (),
        Err(e) => println!("{}", e),
    }
    std::process::exit(0);
}
