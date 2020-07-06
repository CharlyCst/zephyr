mod ast;
mod error;
mod mir;
mod wasm;
mod driver;
mod cli;

fn main() {
    let config = cli::parse();
    let mut driver = driver::Driver::new(config);
    driver.compile();
}

