mod ast;
mod diagnostics;

const PROGRAM: &'static str = r#"
module   foo

fun bar(a: i32) {
    let b = 0x42 * a
}
"#;

fn main() {
    println!("Hello, world!");
    println!("{:?}", ast::scan(PROGRAM));
}
