//#![allow(dead_code)]
#![feature(slice_take)]
#![feature(is_some_with)]
#![feature(result_option_inspect)]

use std::env;

mod generator;
mod lexer;
mod parser;

use crate::generator::gen;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut lexer = Lexer::new_s(args.get(1).unwrap().to_string());
    lexer.run();
    let ast = Parser::new(&lexer).run();
    let asm = gen(&ast);

    println!("{}", asm.to_string());
}
