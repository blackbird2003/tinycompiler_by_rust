mod syntree;
mod lexer;
mod parser;
mod analyzer;
mod asm_templete;
mod asm_generate;

use std::env;
use std::fs;
use std::process;

use crate::lexer::WendLexer;
use crate::parser::WendParser;
use crate::analyzer::decorate;
use crate::asm_generate::asm_generate;
/*
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: cargo run -- path/to/source.wend");
        process::exit(1);
    }
    let src_path = &args[1];
    let src = fs::read_to_string(src_path).unwrap();

    let lexer = WendLexer::new();
    let tokens = lexer.tokenize(&src).unwrap();

    let parser = WendParser::new();
    let mut ast = parser.parse(tokens).unwrap();

    decorate(&mut ast);

    let asm_code = asm_generate(&ast);
    fs::write("out.s", asm_code).unwrap();

    println!("Assembly code generated in out.s");
    println!(
        "Please run it by the following command:\n\
         as --march=i386 --32 -o out.o out.s && \
         ld -m elf_i386 out.o -o out && ./out"
    );
}
*/
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: cargo run -- path/to/source.wend");
        process::exit(1);
    }

    let src_path = &args[1];
    let src = match fs::read_to_string(src_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read {}: {}", src_path, e);
            process::exit(1);
        }
    };

    let lexer = WendLexer::new();
    let tokens = match lexer.tokenize(&src) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexing error: {:?}", e);
            process::exit(1);
        }
    };

    let parser = WendParser::new();
    let mut ast = match parser.parse(tokens) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            process::exit(1);
        }
    };

    decorate(&mut ast);
    
    let asm_code = asm_generate(&ast);

    if let Err(e) = fs::write("out.s", asm_code) {
        eprintln!("Failed to write out.s: {}", e);
        process::exit(1);
    }

    println!("Assembly code generated in out.s");
    println!(
        "Please run it by the following command:\n\
         as --march=i386 --32 -o out.o out.s && \
         ld -m elf_i386 out.o -o out && ./out"
    );
}
