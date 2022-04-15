#![allow(dead_code)]

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;
mod token;

fn main() -> Result<(), common::Error> {
    let demo_src = r#"
        eval('println("hello");')
    "#;

    let mut file = ast::File {
        source: demo_src.chars().collect(),
        exprs: vec![],
    };

    let mut tokenizer = lexer::Lexer::from_chars(file.source.clone());
    let tokens = tokenizer.lex()?;
    file.source = tokenizer.source;
    // dbg!(&tokens);

    file.exprs = parser::parse(&tokens)?;
    // dbg!(&file);

    interpreter::interpret(&file);

    Ok(())
}
