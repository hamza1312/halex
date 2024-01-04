mod args;
mod compiler;
mod lexer;
mod parser;

use std::fs;

use args::HalexArgs;
use clap::Parser as _;
use parser::Parser;

use crate::compiler::{mir::*, Compiler};
fn main() {
    let args = HalexArgs::parse();
    println!("Compiling: {:?}", args.path);
    let input = fs::read_to_string(&args.path).unwrap_or_else(|_| {
        eprintln!("Input file couldn't be read");
        std::process::exit(1);
    });
    let mut parser = Parser::new(&input);
    dbg!(parser.parse());
    Compiler::compile_all(
        vec![
            Mir::ExternFunction {
                name: "puts".into(),
                params: vec![Type::Str],
                return_type: Type::Unit,
            },
            Mir::Call(
                "puts".into(),
                vec![Mir::Lit(Literal::Str("Hello, World".into()))],
            ),
        ],
        &args,
    );
}
