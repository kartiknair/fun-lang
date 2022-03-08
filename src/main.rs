#![allow(dead_code)]

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;
mod token;

fn main() -> Result<(), common::Error> {
    let demo_src = r#"
        नमस्ते := "hello"
        println(नमस्ते)
        जोड़ें := (a, b) -> a + b
        println(जोड़ें(23, 19))
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
