//#![allow(dead_code)]

use std::env;

mod generator;
mod lexer;
mod parser;

use crate::generator::gen;
use crate::lexer::Lexer;
use crate::parser::parse;

fn main() {
    let args: Vec<String> = env::args().collect();

    let toks = Lexer::new(args.get(1).unwrap().to_string()).run();
    let ast = parse(&toks);
    let asm = gen(&ast);

    println!("{}", asm.to_string());
}
