mod syntree;
mod lexer;
mod parser;
mod analyzer;
mod asm_templete;
mod transasm;

/*
main.py in python tinycompiler:

import io, sys
from lexer import WendLexer
from parser import WendParser
from analyzer import decorate
from transasm import transasm

if len(sys.argv)!=2:
    sys.exit('Usage: compiler.py path/source.wend')
try:
    f = open(sys.argv[1], 'r')
    tokens = WendLexer().tokenize(f.read())
    ast = WendParser().parse(tokens)
    decorate(ast)
    asm_code = transasm(ast)
    # print(asm_code)
    # output to ./out.s
    with open('out.s', 'w') as out_file:
        out_file.write(asm_code)
    print('Assembly code generated in out.s')
    print('Please run it by the following command: as --march=i386 --32 -o out.o out.s && ld -m elf_i386 out.o -o out && ./out')

*/
fn main() {
    println!("compiler is not so hard!");
    let source_path = "path/source.wend";
    // ...
}
