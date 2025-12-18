// src/syntree_simple_test.rs

#[cfg(test)]
mod tests {
    use crate::syntree::*;
    use std::fmt::Write as _;
    use std::fs;
    use std::path::PathBuf;
    use std::process::Command;

    fn indent(s: &str) -> String {
        if s.is_empty() {
            return String::new();
        }
        let mut out = String::new();
        for (i, line) in s.lines().enumerate() {
            if i > 0 {
                out.push('\n');
            }
            out.push('\t');
            out.push_str(line);
        }
        out.push('\n');
        out
    }

    fn transpy(f: &Function) -> String {
        let mut out = String::new();

        let funname = &f.name;
        let funargs = f
            .args
            .iter()
            .map(|v| v.name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        writeln!(&mut out, "def {}({}):", funname, funargs).unwrap();

        // function body content (will be indented once)
        let mut body = String::new();

        // allocate locals like Python test: "x = None"
        for v in &f.var {
            writeln!(&mut body, "{} = None", v.name).unwrap();
        }

        // nested functions
        for nf in &f.fun {
            body.push_str(&transpy(nf));
        }

        // statements
        for s in &f.body {
            body.push_str(&stat(s));
        }

        if body.trim().is_empty() {
            body.push_str("pass\n");
        }

        out.push_str(&indent(&body));

        // Python version: if main, add "main()"
        if f.name == "main" {
            out.push_str("main()\n");
        }

        out
    }

    fn stat(s: &Stmt) -> String {
        match s {
            Stmt::Print(p) => {
                let end = if p.newline { "'\\n'" } else { "''" };
                format!("print({}, end={})\n", expr(&p.expr), end)
            }
            Stmt::Return(r) => match &r.expr {
                Some(e) => format!("return {}\n", expr(e)),
                None => "return\n".to_string(),
            },
            Stmt::Assign(a) => format!("{} = {}\n", a.name, expr(&a.expr)),
            Stmt::While(w) => {
                let mut out = String::new();
                writeln!(&mut out, "while {}:", expr(&w.expr)).unwrap();
                let b = w.body.iter().map(stat).collect::<String>();
                out.push_str(&indent(if b.trim().is_empty() { "pass\n" } else { &b }));
                out
            }
            Stmt::IfThenElse(i) => {
                let mut out = String::new();
                writeln!(&mut out, "if {}:", expr(&i.expr)).unwrap();
                let ib = i.ibody.iter().map(stat).collect::<String>();
                out.push_str(&indent(if ib.trim().is_empty() { "pass\n" } else { &ib }));

                out.push_str("else:\n");
                let eb = i.ebody.iter().map(stat).collect::<String>();
                out.push_str(&indent(if eb.trim().is_empty() { "pass\n" } else { &eb }));
                out
            }
            // Python grammar allows a statement-form function call; syntree.rs keeps it.
            Stmt::FunCall(fc) => format!("{}\n", expr(&Expr::FunCall(fc.clone()))),
        }
    }

    fn expr(e: &Expr) -> String {
        match e {
            Expr::ArithOp(op) => {
                let pyop = match op.op.as_str() {
                    "/" => "//",
                    _ => op.op.as_str(),
                };
                format!("({}) {} ({})", expr(&op.left), pyop, expr(&op.right))
            }
            Expr::LogicOp(op) => {
                let pyop = match op.op.as_str() {
                    "&&" => "and",
                    "||" => "or",
                    _ => op.op.as_str(),
                };
                format!("({}) {} ({})", expr(&op.left), pyop, expr(&op.right))
            }
            Expr::Integer(i) => i.value.to_string(),
            Expr::Boolean(b) => b.value.to_string(),
            Expr::StringLit(s) => format!("{:?}", s.value),
            Expr::Var(v) => v.name.clone(),
            Expr::FunCall(c) => {
                let args = c.args.iter().map(expr).collect::<Vec<_>>().join(", ");
                format!("{}({})", c.name, args)
            }
        }
    }

    fn tmp_abs_py_path() -> PathBuf {
        let mut p = PathBuf::from("./"); //std::env::temp_dir();
        // p.push("tinycompiler_abs_test");
        // let _ = fs::create_dir_all(&p);
        p.push("abs.py");
        p
    }

    #[test]
    fn test_abs_syntree_to_python() {
        // optional: make labels deterministic for snapshot-like outputs
        LabelFactory::reset();

        // def abs(x):
        //     if x < 0: return 0-x
        //     else: return x
        let abs_fun = Function::new(
            "abs",
            vec![Var::new("x", deco_t("type", Type::Int))],
            vec![],
            vec![],
            vec![Stmt::IfThenElse(IfThenElse::new(
                Expr::LogicOp(LogicOp::new(
                    "<",
                    Expr::Var(Var::new("x", deco())),
                    Expr::Integer(Integer::new(0, deco())),
                    deco(),
                )),
                vec![Stmt::Return(Return::new(
                    Some(Expr::ArithOp(ArithOp::new(
                        "-",
                        Expr::Integer(Integer::new(0, deco())),
                        Expr::Var(Var::new("x", deco())),
                        deco(),
                    ))),
                    deco(),
                ))],
                vec![Stmt::Return(Return::new(
                    Some(Expr::Var(Var::new("x", deco()))),
                    deco(),
                ))],
                deco(),
            ))],
            deco(),
        );

        // def main():
        //     print(abs(-7))
        //     print(abs(7))
        let main_fun = Function::new(
            "main",
            vec![],
            vec![],
            vec![abs_fun],
            vec![
                Stmt::Print(Print::new(
                    Expr::FunCall(FunCall::new(
                        "abs",
                        vec![Expr::Integer(Integer::new(-7, deco()))],
                        deco(),
                    )),
                    true,
                    deco(),
                )),
                Stmt::Print(Print::new(
                    Expr::FunCall(FunCall::new(
                        "abs",
                        vec![Expr::Integer(Integer::new(7, deco()))],
                        deco(),
                    )),
                    true,
                    deco(),
                )),
            ],
            deco(),
        );

        let program = transpy(&main_fun);

        // write abs.py
        let py_path = tmp_abs_py_path();
        fs::write(&py_path, &program).expect("write abs.py failed");

        // run python and assert output
        let output = Command::new("python3")
            .arg(&py_path)
            .output()
            .expect("failed to run python3");

        assert!(
            output.status.success(),
            "python failed.\n--- program ---\n{}\n--- stderr ---\n{}",
            program,
            String::from_utf8_lossy(&output.stderr)
        );

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        assert_eq!(stdout, "7\n7\n");
    }
}
