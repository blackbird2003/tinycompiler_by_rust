use crate::syntree::*;
use crate::symtable::*;



#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::WendLexer;
    use crate::parser::WendParser;

    #[test]
    fn parse_minimal_main() {
        let src = r#"
        main() {
            int f(int x) {
                return x + 1;
            }
            println f(3);
        }
        "#;

        let lexer = WendLexer::new();
        let tokens = lexer.tokenize(src).unwrap();

        let parser = WendParser::new();
        let ast = parser.parse(tokens).unwrap();

        println!("{:?}", ast);
    }

    #[test]
    fn parse_expr_precedence() {
        let src = r#"
        main() {
            int x;
            int f(int x) {
                int y;
                y = x + 1 * 2;
                return y;
            }
            x = f(3) + 1;
            println x;
        }
        "#;
        let lexer = WendLexer::new();
        let tokens = lexer.tokenize(src).unwrap();
        let parser = WendParser::new();
        let ast = parser.parse(tokens).unwrap();
        let decorated_ast = decorate(ast);
        println!("{:?}", decorated_ast);
    }
}


/*
Function { name: "main", args: [], var: [Var { name: "x", deco: {"type": Ty(Int), "lineno": Int(2), "scope": Int(0), "offset": Int(0)} }], fun: [Function { name: "f", args: [Var { name: "x", deco: {"lineno": Int(3), "type": Ty(Int), "scope": Int(1), "offset": Int(0)} }], var: [Var { name: "y", deco: {"lineno": Int(4), "type": Ty(Int), "scope": Int(1), "offset": Int(1)} }], fun: [], body: [Assign(Assign { name: "y", expr: ArithOp(ArithOp { op: "+", left: Var(Var { name: "x", deco: {"lineno": Int(5), "type": Ty(Int), "scope": Int(1), "offset": Int(0)} }), right: ArithOp(ArithOp { op: "*", left: Integer(Integer { value: 1, deco: {"lineno": Int(5), "type": Ty(Int)} }), right: Integer(Integer { value: 2, deco: {"lineno": Int(5), "type": Ty(Int)} }), deco: {"lineno": Int(5), "type": Ty(Int)} }), deco: {"lineno": Int(5), "type": Ty(Int)} }), deco: {"lineno": Int(5), "type": Ty(Int), "scope": Int(1), "offset": Int(1)} }), Return(Return { expr: Some(Var(Var { name: "y", deco: {"lineno": Int(6), "type": Ty(Int), "scope": Int(1), "offset": Int(1)} })), deco: {"lineno": Int(6)} })], deco: {"lineno": Int(3), "type": Ty(Int), "label": Str("f_uniqstr3"), "scope": Int(1), "var_cnt": Int(2)} }], body: [Assign(Assign { name: "x", expr: ArithOp(ArithOp { op: "+", left: FunCall(FunCall { name: "f", args: [Integer(Integer { value: 3, deco: {"lineno": Int(8), "type": Ty(Int)} })], deco: {"lineno": Int(8), "label": Str("f_uniqstr3"), "scope": Int(1), "var_cnt": Int(2), "type": Ty(Int)} }), right: Integer(Integer { value: 1, deco: {"lineno": Int(8), "type": Ty(Int)} }), deco: {"lineno": Int(8), "type": Ty(Int)} }), deco: {"lineno": Int(8), "type": Ty(Int), "scope": Int(0), "offset": Int(0)} }), Print(Print { expr: Var(Var { name: "x", deco: {"lineno": Int(9), "type": Ty(Int), "scope": Int(0), "offset": Int(0)} }), newline: true, deco: {"lineno": Int(9)} })], deco: {"label": Str("main_uniqstr4"), "type": Ty(Void), "lineno": Int(1), "scope": Int(0), "var_cnt": Int(1), "strings": Set([]), "scope_cnt": Int(2)} }
*/

