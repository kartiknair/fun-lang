#![allow(dead_code)]

mod ast;
mod common;
mod interpreter;
mod lexer;
mod parser;
mod token;

fn main() -> Result<(), common::Error> {
    let demo_src = r#"
        foo := "नमस्ते"
        obj := {x: 24, y: 32}
        for c in enumerate(foo) {
            print(c)
        }
        for el in obj {
            print(el)
        }
        print(type(obj))
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
