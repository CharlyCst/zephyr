use std::env;

mod ast;
mod error;
mod mir;
mod wasm;
mod driver;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut output_path = "a.wasm";
    if args.len() == 3 {
        output_path = &args[2];
    } else if args.len() != 2 {
        println!("Usage: fork <file> [out]");
        std::process::exit(64);
    }
    // let code = fs::read_to_string(&args[1]).expect("File not found");
    compile(args[1].clone(), output_path.to_string());
}

fn compile(input_path: String, output_path: String) {
    let mut driver = driver::Driver::new(input_path, output_path);
    driver.compile();
}
