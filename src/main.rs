mod args;
mod lexer;
mod parser;

use std::fs;

use args::HalexArgs;
use clap::Parser as _;
use parser::Parser;
fn main() {
    let args = HalexArgs::parse();
    println!("Compiling: {:?}", args.path);
    let input = fs::read_to_string(&args.path).unwrap_or_else(|_| {
        eprintln!("Input file couldn't be read");
        std::process::exit(1);
    });
    let mut parser = Parser::new(&input);
    dbg!(parser.parse());
}
