#![allow(dead_code)]

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;
mod token;

fn main() -> Result<(), common::Error> {
    let demo_src = r#"
        fib := (n) -> {
            if n < 2 {
                n
            } else {
                fib(n-1) + fib(n-2)
            }
        }

        println(fib(12))
    "#;

    let mut file = ast::File {
        source: demo_src.chars().collect(),
        exprs: vec![],
    };

    let tokens = lexer::lex(file.source.clone())?;
    // dbg!(&tokens);

    file.exprs = parser::parse(&tokens)?;
    // dbg!(&file);

    interpreter::interpret(&file);

    Ok(())
}
