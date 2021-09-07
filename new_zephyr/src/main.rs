mod ast;
mod diagnostics;
mod syntax;
// mod id;
// mod location;

// const PROGRAM: &'static str = r#"
// module   foo

// use std::string

// /// A super function!
// fun bar(a: i32) {
//     let b = 0x42 * a
//     let c = 012
//     let s = "test :)"
// }
// "#;

const PROGRAM: &'static str = r#"runtime // hello!
module     
demo // Hi!

use std::str

"#;

fn main() {
    let (tokens, errors) = ast::scan(PROGRAM);
    println!("{:?}\n\n{:?}\n", tokens, errors);
    let (syntax_tree, errors) = ast::parse(tokens);
    println!("{:?}\n\n{:?}\n", syntax_tree, errors);
    assert_eq!(&format!("{}", syntax_tree), PROGRAM);
}
