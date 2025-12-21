mod lexer;
mod syntree;
mod syntree_simple_test;
mod parser;
mod analyzer;
mod transasm;

/*
main() in python tinycompiler

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
    # as --march=i386 --32 -o out.o out.s && ld -m elf_i386 out.o -o out && ./out

*/
fn main() {
    println!("compiler is not so hard!");
    source_path: &str = "path/source.wend";
    // ...
}
