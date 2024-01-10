mod args;
mod compiler;
mod lexer;
mod lower;
mod parser;

use std::{fs, time::Instant};

use args::HalexArgs;
use clap::Parser as _;
use miette::{miette, Severity};
use parser::Parser;

use crate::{compiler::Compiler, lower::lower_ast};
fn main() {
    let args = HalexArgs::parse();
    let start = Instant::now();
    println!("Compiling: {:?}", args.path);
    let input = fs::read_to_string(&args.path).unwrap_or_else(|_| {
        eprintln!("Input file couldn't be read");
        std::process::exit(1);
    });
    let mut parser = Parser::new(&input);
    let context = inkwell::context::Context::create();
    let mut compiler = Compiler::new(lower_ast(parser.parse()), &context, &args);
    compiler.compile();
    compiler.finish();
    println!(
        "{:?}",
        miette!(
            severity = Severity::Advice,
            "Compiled successfully in {}",
            format_duration(start.elapsed())
        )
    );
}

fn format_duration(duration: std::time::Duration) -> String {
    let total_secs = duration.as_secs();
    let nano_secs = duration.subsec_nanos() as f64 / 1_000_000_000.0;

    if total_secs > 0 {
        if total_secs >= 60 {
            let mins = total_secs / 60;
            let secs = total_secs % 60;
            format!("{}m {}s", mins, secs)
        } else {
            format!("{:.2}s", total_secs as f64 + nano_secs)
        }
    } else if duration.subsec_millis() > 0 {
        format!(
            "{:.2}ms",
            duration.subsec_millis() as f64 + nano_secs * 1_000.0
        )
    } else if duration.subsec_micros() > 0 {
        format!(
            "{:.2}μs",
            duration.subsec_micros() as f64 + nano_secs * 1_000_000.0
        )
    } else {
        format!("{}ns", duration.subsec_nanos())
    }
}
