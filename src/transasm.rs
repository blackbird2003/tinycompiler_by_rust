use std::collections::{HashMap, HashSet};
use crate::asm_templete::TEMPLATES;
use crate::syntree::{
    Deco, DecoValue, Expr, Function, LabelFactory, Stmt, Type, Var,
};

/// Render a template by simple `{key}` replacement.
/// (Enough for this project; templates are small and keys are unique.)
fn render(template: &str, items: &[(&str, String)]) -> String {
    let mut s = template.to_string();
    for (k, v) in items {
        s = s.replace(&format!("{{{}}}", k), v);
    }
    s
}

fn get_i(deco: &Deco, key: &str) -> i32 {
    match deco.get(key) {
        Some(DecoValue::Int(v)) => *v as i32,
        other => panic!("missing/invalid deco int key `{}`: {:?}", key, other),
    }
}

fn get_s(deco: &Deco, key: &str) -> String {
    match deco.get(key) {
        Some(DecoValue::Str(v)) => v.clone(),
        other => panic!("missing/invalid deco str key `{}`: {:?}", key, other),
    }
}

fn get_ty(deco: &Deco, key: &str) -> Type {
    match deco.get(key) {
        Some(DecoValue::Ty(t)) => *t,
        other => panic!("missing/invalid deco type key `{}`: {:?}", key, other),
    }
}

fn get_stringset(deco: &Deco, key: &str) -> HashSet<(String, String)> {
    match deco.get(key) {
        Some(DecoValue::StringSet(set)) => set.clone(),
        other => panic!("missing/invalid deco stringset key `{}`: {:?}", key, other),
    }
}

/// Escape for GNU as `.ascii "..."`.
/// Keep it conservative: escape backslash, quote, and map real newlines/tabs.
fn escape_ascii(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            _ => out.push(ch),
        }
    }
    out
}

pub fn transasm(main_fun: &Function) -> String {
    // strings: ''.join([templates['ascii']... for label,string in n.deco['strings']])
    let mut strings_asm = String::new();
    let strings = get_stringset(&main_fun.deco, "strings");
    for (label, string) in strings {
        strings_asm.push_str(&render(
            TEMPLATES.ascii,
            &[
                ("label", label),
                ("string", escape_ascii(&string)),
            ],
        ));
    }

    let display_size = get_i(&main_fun.deco, "scope_cnt") * 4;
    let offset = get_i(&main_fun.deco, "scope") * 4;
    let main_label = get_s(&main_fun.deco, "label");
    let varsize = (main_fun.var.len() as i32) * 4;
    let functions = gen_fun(main_fun);

    render(
        TEMPLATES.program,
        &[
            ("strings", strings_asm),
            ("display_size", display_size.to_string()),
            ("offset", offset.to_string()),
            ("main", main_label),
            ("varsize", varsize.to_string()),
            ("functions", functions),
        ],
    )
}

fn gen_fun(n: &Function) -> String {
    let label = get_s(&n.deco, "label");
    let mut nested = String::new();
    for f in &n.fun {
        nested.push_str(&gen_fun(f));
    }
    let mut body = String::new();
    for s in &n.body {
        body.push_str(&gen_stmt(s));
    }
    format!("{label}:\n{body}\n\tret\n{nested}\n")
}

fn gen_stmt(n: &Stmt) -> String {
    match n {
        Stmt::Print(p) => {
            let ty = get_ty(&p.expr_deco(), "type");
            let asm = match ty {
                Type::Int => render(TEMPLATES.print_int, &[("expr", gen_expr(&p.expr))]),
                Type::Bool => render(TEMPLATES.print_bool, &[("expr", gen_expr(&p.expr))]),
                Type::String => {
                    let label = get_s(&p.expr_deco(), "label");
                    render(TEMPLATES.print_string, &[("label", label)])
                }
                other => panic!("Unknown expression type for print: {:?}", other),
            };
            if p.newline { format!("{asm}{}", TEMPLATES.print_linebreak) } else { asm }
        }
        Stmt::Return(r) => {
            if let Some(expr) = &r.expr {
                let ty = get_ty(&expr_deco(expr), "type");
                if ty != Type::Void {
                    return format!("{}\tret\n", gen_expr(expr));
                }
            }
            "\tret\n".to_string()
        }
        Stmt::Assign(a) => {
            let scope = get_i(&a.deco, "scope") * 4;
            let variable = get_i(&a.deco, "offset") * 4;
            render(
                TEMPLATES.assign,
                &[
                    ("expression", gen_expr(&a.expr)),
                    ("scope", scope.to_string()),
                    ("variable", variable.to_string()),
                ],
            )
        }
        Stmt::While(w) => {
            let label1 = LabelFactory::new_label();
            let label2 = LabelFactory::new_label();
            let mut body = String::new();
            for s in &w.body {
                body.push_str(&gen_stmt(s));
            }
            render(
                TEMPLATES.while_loop,
                &[
                    ("condition", gen_expr(&w.expr)),
                    ("label1", label1),
                    ("label2", label2),
                    ("body", body),
                ],
            )
        }
        Stmt::IfThenElse(it) => {
            let label1 = LabelFactory::new_label();
            let label2 = LabelFactory::new_label();
            let mut ibody = String::new();
            for s in &it.ibody {
                ibody.push_str(&gen_stmt(s));
            }
            let mut ebody = String::new();
            for s in &it.ebody {
                ebody.push_str(&gen_stmt(s));
            }
            render(
                TEMPLATES.ifthenelse,
                &[
                    ("condition", gen_expr(&it.expr)),
                    ("label1", label1),
                    ("label2", label2),
                    ("ibody", ibody),
                    ("ebody", ebody),
                ],
            )
        }
        Stmt::FunCall(fc) => gen_fun_call(fc),
    }
}

fn gen_expr(n: &Expr) -> String {
    match n {
        Expr::ArithOp(a) => gen_binop(&a.op, &a.left, &a.right),
        Expr::LogicOp(l) => gen_binop(&l.op, &l.left, &l.right),
        Expr::Integer(i) => format!("\tmovl ${}, %eax\n", i.value as i32),
        Expr::Boolean(b) => format!("\tmovl ${}, %eax\n", if b.value { 1 } else { 0 }),
        Expr::StringLit(s) => {
            // Not used by python for printing (print_string uses label directly),
            // but keeping it usable makes codegen robust.
            let label = get_s(&s.deco, "label");
            format!("\tmovl ${}, %eax\n", label)
        }
        Expr::Var(v) => {
            let scope = get_i(&v.deco, "scope") * 4;
            let variable = get_i(&v.deco, "offset") * 4;
            render(
                TEMPLATES.var,
                &[
                    ("scope", scope.to_string()),
                    ("variable", variable.to_string()),
                ],
            )
        }
        Expr::FunCall(fc) => gen_fun_call(fc),
    }
}

fn gen_binop(op: &str, left: &Expr, right: &Expr) -> String {
    let mut args = String::new();
    args.push_str(&gen_expr(left));
    args.push_str("\tpushl %eax\n");
    args.push_str(&gen_expr(right));
    args.push_str("\tmovl %eax, %ebx\n\tpopl %eax\n");

    let pyeq1: HashMap<&str, &str> = HashMap::from([
        ("+", "addl"),
        ("-", "subl"),
        ("*", "imull"),
        ("||", "orl"),
        ("&&", "andl"),
    ]);
    let pyeq2: HashMap<&str, &str> = HashMap::from([
        ("<=", "jle"),
        ("<", "jl"),
        (">=", "jge"),
        (">", "jg"),
        ("==", "je"),
        ("!=", "jne"),
    ]);

    if let Some(ins) = pyeq1.get(op) {
        return format!("{args}\t{ins} %ebx, %eax\n");
    }
    if let Some(jcc) = pyeq2.get(op) {
        return format!(
            "{args}\tcmp %ebx, %eax\n\tmovl $1, %eax\n\t{jcc} 1f\n\txorl %eax, %eax\n1:\n"
        );
    }
    if op == "/" {
        return format!("{args}\tcdq\n\tidivl %ebx, %eax\n");
    }
    if op == "%" {
        return format!("{args}\tcdq\n\tidivl %ebx, %eax\n\tmovl %edx, %eax\n");
    }
    panic!("Unknown binary operation: {op}");
}

fn gen_fun_call(n: &crate::syntree::FunCall) -> String {
    // allocargs = ''.join(['%s\tpushl %%eax\n' % stat(a) for a in n.args])
    let mut allocargs = String::new();
    for a in &n.args {
        allocargs.push_str(&gen_expr(a));
        allocargs.push_str("\tpushl %eax\n");
    }

    let var_cnt = get_i(&n.deco, "var_cnt");
    let varsize = var_cnt * 4;
    let disphead = var_cnt * 4 + (n.args.len() as i32) * 4 - 4;
    let scope = get_i(&n.deco, "scope") * 4;
    let funlabel = get_s(&n.deco, "label");

    render(
        TEMPLATES.funcall,
        &[
            ("allocargs", allocargs),
            ("varsize", varsize.to_string()),
            ("disphead", disphead.to_string()),
            ("scope", scope.to_string()),
            ("funlabel", funlabel),
        ],
    )
}

/// Small helpers: fetch expr deco (since Expr is enum).
fn expr_deco(e: &Expr) -> &Deco {
    match e {
        Expr::ArithOp(x) => &x.deco,
        Expr::LogicOp(x) => &x.deco,
        Expr::Integer(x) => &x.deco,
        Expr::Boolean(x) => &x.deco,
        Expr::StringLit(x) => &x.deco,
        Expr::Var(x) => &x.deco,
        Expr::FunCall(x) => &x.deco,
    }
}

/// Provide `expr_deco()` on Print-like nodes without exposing internals.
trait PrintExprDeco {
    fn expr_deco(&self) -> &Deco;
}
impl PrintExprDeco for crate::syntree::Print {
    fn expr_deco(&self) -> &Deco { expr_deco(&self.expr) }
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::WendLexer;
    use crate::parser::WendParser;
    use crate::analyzer::decorate;
    #[test]
    fn parse_expr_precedence() {
        let src = r#"main() {
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
        let mut ast = parser.parse(tokens).unwrap();
        decorate(&mut ast);
        let asm_code = transasm(&mut ast);
        println!("{}", asm_code);
    }
}


