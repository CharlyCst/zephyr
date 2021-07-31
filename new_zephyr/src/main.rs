mod ast;
mod diagnostics;

const PROGRAM: &'static str = r#"
module   foo

fun bar(a: i32) {
    let b = 0x42 * a
    let c = 012a
    let s = "test :)"
}
"#;

fn main() {
    println!("Hello, world!");
    println!("{:?}", ast::scan(PROGRAM));
}
