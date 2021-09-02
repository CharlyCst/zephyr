mod ast;
mod diagnostics;
mod syntax;
// mod id;
// mod location;

const PROGRAM: &'static str = r#"
module   foo

use std::string

fun bar(a: i32) {
    let b = 0x42 * a
    let c = 012
    let s = "test :)"
}
"#;

fn main() {
    let (tokens, errors) = ast::scan(PROGRAM);
    println!("{:?}", tokens.tokens());
    println!("{}\n\n{:?}", tokens, errors);
}
