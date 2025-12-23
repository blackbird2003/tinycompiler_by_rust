// src/analyzer.rs
use std::collections::{HashMap, HashSet};

use crate::syntree::{
    Deco, DecoValue, Expr, Function, LogicOp, Stmt, Type, Var,
};

/// 简单的语义错误：为了对齐 Python 行为，直接 panic 也行；
/// 这里用 Result 内部传播，再由 decorate() panic。
#[derive(Debug, Clone)]
pub struct AnalyzerError(pub String);

impl std::fmt::Display for AnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
impl std::error::Error for AnalyzerError {}

type AResult<T> = Result<T, AnalyzerError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunSignature {
    name: String,
    arg_types: Vec<Type>,
}

#[derive(Debug, Clone)]
struct FunCtx {
    scope: usize,
    ret_type: Type,
    var_cnt: usize,
}

/// Python 版 SymbolTable 的 Rust 对应实现：
/// - variables: Vec<Map<name, Deco>>
/// - functions: Vec<Map<signature, Deco>>
/// - ret_stack: Vec<FunCtx>
/// - scope_cnt: usize
/// - main_strings: 指向 main.deco["strings"] 的可变指针（为了在任意嵌套函数里收集字符串）
struct SymbolTable {
    variables: Vec<HashMap<String, Deco>>,
    functions: Vec<HashMap<FunSignature, Deco>>,
    ret_stack: Vec<FunCtx>,
    scope_cnt: usize,

    // 指向 main.deco["strings"] 里的 HashSet<(label,value)>
    main_strings: *mut HashSet<(String, String)>,
}

impl SymbolTable {
    fn new(main_strings: *mut HashSet<(String, String)>) -> Self {
        Self {
            variables: vec![HashMap::new()],
            functions: vec![HashMap::new()],
            ret_stack: Vec::new(),
            scope_cnt: 0,
            main_strings,
        }
    }

    fn push_scope(&mut self, fun_deco: &mut Deco) -> AResult<()> {
        self.variables.push(HashMap::new());
        self.functions.push(HashMap::new());

        let scope = get_int(fun_deco, "scope")
            .ok_or_else(|| AnalyzerError("Function deco missing 'scope'".into()))? as usize;

        let ret_type = get_type(fun_deco)
            .ok_or_else(|| AnalyzerError("Function deco missing 'type'".into()))?;

        // Python: deco['var_cnt']=0
        fun_deco.insert("var_cnt".into(), DecoValue::Int(0));
        self.ret_stack.push(FunCtx {
            scope,
            ret_type,
            var_cnt: 0,
        });
        Ok(())
    }

    fn pop_scope(&mut self) {
        self.variables.pop();
        self.functions.pop();
        self.ret_stack.pop();
    }

    fn add_fun(&mut self, name: &str, arg_types: &[Type], fun_deco: &mut Deco) -> AResult<()> {
        let sig = FunSignature {
            name: name.to_string(),
            arg_types: arg_types.to_vec(),
        };

        let cur = self
            .functions
            .last_mut()
            .expect("functions stack must be non-empty");

        if cur.contains_key(&sig) {
            return Err(AnalyzerError(format!(
                "Double declaration of the function {} {:?}",
                name, arg_types
            )));
        }

        // Python: deco['scope'] = self.scope_cnt; self.scope_cnt += 1
        fun_deco.insert("scope".into(), DecoValue::Int(self.scope_cnt as i64));
        self.scope_cnt += 1;

        cur.insert(sig, fun_deco.clone());
        Ok(())
    }

    fn update_fun(&mut self, name: &str, arg_types: &[Type], fun_deco: &Deco) {
        let sig = FunSignature {
            name: name.to_string(),
            arg_types: arg_types.to_vec(),
        };
        if let Some(cur) = self.functions.last_mut() {
            if cur.contains_key(&sig) {
                cur.insert(sig, fun_deco.clone());
            }
        }
    }

    fn add_var(&mut self, name: &str, var_deco: &mut Deco) -> AResult<()> {
        let cur = self
            .variables
            .last_mut()
            .expect("variables stack must be non-empty");
        if cur.contains_key(name) {
            return Err(AnalyzerError(format!(
                "Double declaration of the variable {}",
                name
            )));
        }

        let ctx = self
            .ret_stack
            .last_mut()
            .ok_or_else(|| AnalyzerError("add_var called outside of any function scope".into()))?;

        // Python:
        // deco['scope']  = ret_stack[-1]['scope']
        // deco['offset'] = ret_stack[-1]['var_cnt']
        // ret_stack[-1]['var_cnt'] += 1
        var_deco.insert("scope".into(), DecoValue::Int(ctx.scope as i64));
        var_deco.insert("offset".into(), DecoValue::Int(ctx.var_cnt as i64));

        ctx.var_cnt += 1;

        cur.insert(name.to_string(), var_deco.clone());
        Ok(())
    }

    fn find_var(&self, name: &str) -> AResult<Deco> {
        for scope in self.variables.iter().rev() {
            if let Some(d) = scope.get(name) {
                return Ok(d.clone());
            }
        }
        Err(AnalyzerError(format!(
            "No declaration for the variable {}",
            name
        )))
    }

    fn find_fun(&self, name: &str, arg_types: &[Type]) -> AResult<Deco> {
        let sig = FunSignature {
            name: name.to_string(),
            arg_types: arg_types.to_vec(),
        };
        for scope in self.functions.iter().rev() {
            if let Some(d) = scope.get(&sig) {
                return Ok(d.clone());
            }
        }
        Err(AnalyzerError(format!(
            "No declaration for the function {} {:?}",
            name, arg_types
        )))
    }

    fn current_fun_ctx(&self) -> AResult<&FunCtx> {
        self.ret_stack
            .last()
            .ok_or_else(|| AnalyzerError("ret_stack empty".into()))
    }

    fn current_fun_ctx_mut(&mut self) -> AResult<&mut FunCtx> {
        self.ret_stack
            .last_mut()
            .ok_or_else(|| AnalyzerError("ret_stack empty".into()))
    }

    /// Python: symtable.ret_stack[1]['strings'].add((label, value))
    fn add_string_constant(&mut self, label: String, value: String) -> AResult<()> {
        if self.main_strings.is_null() {
            return Err(AnalyzerError("main_strings pointer not initialized".into()));
        }
        unsafe {
            (*self.main_strings).insert((label, value));
        }
        Ok(())
    }
}

// -----------------------------
// Public entry: decorate
// -----------------------------

pub fn decorate(ast: &mut Function) {
    if let Err(e) = decorate_impl(ast) {
        panic!("{}", e);
    }
}

fn decorate_impl(ast: &mut Function) -> AResult<()> {
    // Python entrypoint check:
    // if not Function or name != main or type != VOID or len(args)>0: raise
    if ast.name != "main" {
        return Err(AnalyzerError("Cannot find a valid entry point".into()));
    }
    if !ast.args.is_empty() {
        return Err(AnalyzerError("Cannot find a valid entry point".into()));
    }
    let ret_ty = get_type(&ast.deco).ok_or_else(|| {
        AnalyzerError("Cannot find a valid entry point (missing return type)".into())
    })?;
    if ret_ty != Type::Void {
        return Err(AnalyzerError("Cannot find a valid entry point".into()));
    }

    // main.deco['strings'] = set()
    ast.deco
        .insert("strings".into(), DecoValue::StringSet(HashSet::new()));

    // 取到 strings 集合的可变指针，供任意嵌套处 StringLit 收集
    let strings_ptr: *mut HashSet<(String, String)> = match ast.deco.get_mut("strings") {
        Some(DecoValue::StringSet(set)) => set as *mut HashSet<(String, String)>,
        _ => return Err(AnalyzerError("Failed to init main strings set".into())),
    };

    let mut sym = SymbolTable::new(strings_ptr);

    // symtable.add_fun(main)
    sym.add_fun(&ast.name, &[], &mut ast.deco)?;

    // Python 调两次 process_scope（照做）
    process_scope(ast, &mut sym)?;
    process_scope(ast, &mut sym)?;

    // main.deco['scope_cnt'] = sym.scope_cnt
    ast.deco
        .insert("scope_cnt".into(), DecoValue::Int(sym.scope_cnt as i64));

    Ok(())
}

// -----------------------------
// process_scope (function-level)
// -----------------------------

fn process_scope(fun: &mut Function, sym: &mut SymbolTable) -> AResult<()> {
    sym.push_scope(&mut fun.deco)?;

    // args
    for v in fun.args.iter_mut() {
        sym.add_var(&v.name, &mut v.deco)?;
    }
    // locals
    for v in fun.var.iter_mut() {
        sym.add_var(&v.name, &mut v.deco)?;
    }

    // nested fun: first add symbols
    for f in fun.fun.iter_mut() {
        let arg_types = collect_arg_types(&f.args)?;
        sym.add_fun(&f.name, &arg_types, &mut f.deco)?;
    }

    // then process bodies
    for f in fun.fun.iter_mut() {
        process_scope(f, sym)?;

        // 把处理后的 f.deco（含 var_cnt 等）写回当前函数层的 functions 表
        let arg_types = collect_arg_types(&f.args)?;
        sym.update_fun(&f.name, &arg_types, &f.deco);
    }

    // statements
    for s in fun.body.iter_mut() {
        process_stmt(s, sym)?;
    }

    // 关键：把当前函数最终 var_cnt 写回 fun.deco（Python 是同一个 dict，天然同步）
    let ctx = sym.current_fun_ctx()?.clone();
    fun.deco
        .insert("var_cnt".into(), DecoValue::Int(ctx.var_cnt as i64));

    sym.pop_scope();
    Ok(())
}

// -----------------------------
// process_instruction (stmt/expr)
// -----------------------------

fn process_stmt(s: &mut Stmt, sym: &mut SymbolTable) -> AResult<()> {
    match s {
        Stmt::Print(p) => {
            process_expr(&mut p.expr, sym)?;
            Ok(())
        }
        Stmt::Return(r) => {
            if let Some(e) = r.expr.as_mut() {
                process_expr(e, sym)?;
                let ctx = sym.current_fun_ctx()?;
                let ety = get_type_expr(e).ok_or_else(|| {
                    AnalyzerError(format!(
                        "Missing expr type in return, line {}",
                        get_lineno(&r.deco).unwrap_or(-1)
                    ))
                })?;
                if ctx.ret_type != ety {
                    return Err(AnalyzerError(format!(
                        "Incompatible types in return statement, line {}",
                        get_lineno(&r.deco).unwrap_or(-1)
                    )));
                }
            } else {
                // Python: if expr is None: return (TODO non-void check)
            }
            Ok(())
        }
        Stmt::Assign(a) => {
            process_expr(&mut a.expr, sym)?;
            let vd = sym.find_var(&a.name)?;
            merge_into(&mut a.deco, &vd);

            let lhs_ty = get_type(&a.deco).ok_or_else(|| {
                AnalyzerError(format!(
                    "Missing lhs type in assignment, line {}",
                    get_lineno(&a.deco).unwrap_or(-1)
                ))
            })?;
            let rhs_ty = get_type_expr(&a.expr).ok_or_else(|| {
                AnalyzerError(format!(
                    "Missing rhs type in assignment, line {}",
                    get_lineno(&a.deco).unwrap_or(-1)
                ))
            })?;
            if lhs_ty != rhs_ty {
                return Err(AnalyzerError(format!(
                    "Incompatible types in assignment statement, line {}",
                    get_lineno(&a.deco).unwrap_or(-1)
                )));
            }
            Ok(())
        }
        Stmt::While(w) => {
            process_expr(&mut w.expr, sym)?;
            let ty = get_type_expr(&w.expr).ok_or_else(|| {
                AnalyzerError(format!(
                    "Missing condition type in while, line {}",
                    get_lineno(&w.deco).unwrap_or(-1)
                ))
            })?;
            if ty != Type::Bool {
                return Err(AnalyzerError(format!(
                    "Non-boolean expression in while statement, line {}",
                    get_lineno(&w.deco).unwrap_or(-1)
                )));
            }
            for st in w.body.iter_mut() {
                process_stmt(st, sym)?;
            }
            Ok(())
        }
        Stmt::IfThenElse(it) => {
            process_expr(&mut it.expr, sym)?;
            let ty = get_type_expr(&it.expr).ok_or_else(|| {
                AnalyzerError(format!(
                    "Missing condition type in if, line {}",
                    get_lineno(&it.deco).unwrap_or(-1)
                ))
            })?;
            if ty != Type::Bool {
                return Err(AnalyzerError(format!(
                    "Non-boolean expression in if statement, line {}",
                    get_lineno(&it.deco).unwrap_or(-1)
                )));
            }
            for st in it.ibody.iter_mut() {
                process_stmt(st, sym)?;
            }
            for st in it.ebody.iter_mut() {
                process_stmt(st, sym)?;
            }
            Ok(())
        }
        Stmt::FunCall(fc) => {
            // stmt-form call
            process_fun_call(fc, sym)?;
            Ok(())
        }
    }
}

fn process_expr(e: &mut Expr, sym: &mut SymbolTable) -> AResult<()> {
    match e {
        Expr::ArithOp(a) => {
            process_expr(&mut a.left, sym)?;
            process_expr(&mut a.right, sym)?;
            let lt = get_type_expr(&a.left).unwrap_or(Type::Void);
            let rt = get_type_expr(&a.right).unwrap_or(Type::Void);
            if lt != Type::Int || rt != Type::Int {
                return Err(AnalyzerError(format!(
                    "Arithmetic operation over non-integer type in line {}",
                    get_lineno(&a.deco).unwrap_or(-1)
                )));
            }
            Ok(())
        }
        Expr::LogicOp(l) => {
            process_expr(&mut l.left, sym)?;
            process_expr(&mut l.right, sym)?;
            let lt = get_type_expr(&l.left).unwrap_or(Type::Void);
            let rt = get_type_expr(&l.right).unwrap_or(Type::Void);

            let op = l.op.as_str();
            let bad = (lt != rt)
                || ((op == "<=" || op == "<" || op == ">=" || op == ">") && lt != Type::Int)
                || ((op == "&&" || op == "||") && lt != Type::Bool);

            if bad {
                return Err(AnalyzerError(format!(
                    "Boolean operation over incompatible types in line {}",
                    get_lineno(&l.deco).unwrap_or(-1)
                )));
            }
            Ok(())
        }
        Expr::Var(v) => {
            let vd = sym.find_var(&v.name)?;
            merge_into(&mut v.deco, &vd);
            Ok(())
        }
        Expr::FunCall(fc) => process_fun_call(fc, sym),
        Expr::StringLit(s) => {
            // Python: add to main strings set
            let label = match s.deco.get("label") {
                Some(DecoValue::Str(x)) => x.clone(),
                _ => {
                    return Err(AnalyzerError(format!(
                        "String literal missing label, line {}",
                        get_lineno(&s.deco).unwrap_or(-1)
                    )))
                }
            };
            sym.add_string_constant(label, s.value.clone())?;
            Ok(())
        }
        Expr::Integer(_) | Expr::Boolean(_) => Ok(()),
    }
}

fn process_fun_call(fc: &mut crate::syntree::FunCall, sym: &mut SymbolTable) -> AResult<()> {
    for a in fc.args.iter_mut() {
        process_expr(a, sym)?;
    }
    let arg_types: Vec<Type> = fc
        .args
        .iter()
        .map(|x| get_type_expr(x).unwrap_or(Type::Void))
        .collect();

    let fd = sym.find_fun(&fc.name, &arg_types)?;
    merge_into(&mut fc.deco, &fd);
    Ok(())
}

// -----------------------------
// Helpers
// -----------------------------

fn collect_arg_types(args: &[Var]) -> AResult<Vec<Type>> {
    let mut out = Vec::with_capacity(args.len());
    for v in args {
        let t = get_type(&v.deco).ok_or_else(|| {
            AnalyzerError(format!(
                "Function argument {} missing type",
                v.name
            ))
        })?;
        out.push(t);
    }
    Ok(out)
}

fn merge_into(dst: &mut Deco, src: &Deco) {
    for (k, v) in src.iter() {
        dst.insert(k.clone(), v.clone());
    }
}

fn get_int(d: &Deco, key: &str) -> Option<i64> {
    match d.get(key) {
        Some(DecoValue::Int(x)) => Some(*x),
        _ => None,
    }
}

fn get_lineno(d: &Deco) -> Option<i64> {
    get_int(d, "lineno")
}

fn get_type(d: &Deco) -> Option<Type> {
    match d.get("type") {
        Some(DecoValue::Ty(t)) => Some(*t),
        _ => None,
    }
}

fn get_type_expr(e: &Expr) -> Option<Type> {
    match e {
        Expr::ArithOp(a) => get_type(&a.deco),
        Expr::LogicOp(l) => get_type(&l.deco),
        Expr::Integer(i) => get_type(&i.deco),
        Expr::Boolean(b) => get_type(&b.deco),
        Expr::StringLit(s) => get_type(&s.deco),
        Expr::Var(v) => get_type(&v.deco),
        Expr::FunCall(fc) => get_type(&fc.deco),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::WendLexer;
    use crate::parser::WendParser;

    #[test]
    fn parse_minimal_main() {
        let src = r#"main() {
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
        println!("{:?}", ast);
    }
}


/*
Function { name: "main", args: [], var: [Var { name: "x", deco: {"type": Ty(Int), "lineno": Int(2), "scope": Int(0), "offset": Int(0)} }], fun: [Function { name: "f", args: [Var { name: "x", deco: {"lineno": Int(3), "type": Ty(Int), "scope": Int(1), "offset": Int(0)} }], var: [Var { name: "y", deco: {"lineno": Int(4), "type": Ty(Int), "scope": Int(1), "offset": Int(1)} }], fun: [], body: [Assign(Assign { name: "y", expr: ArithOp(ArithOp { op: "+", left: Var(Var { name: "x", deco: {"lineno": Int(5), "type": Ty(Int), "scope": Int(1), "offset": Int(0)} }), right: ArithOp(ArithOp { op: "*", left: Integer(Integer { value: 1, deco: {"lineno": Int(5), "type": Ty(Int)} }), right: Integer(Integer { value: 2, deco: {"lineno": Int(5), "type": Ty(Int)} }), deco: {"lineno": Int(5), "type": Ty(Int)} }), deco: {"lineno": Int(5), "type": Ty(Int)} }), deco: {"lineno": Int(5), "type": Ty(Int), "scope": Int(1), "offset": Int(1)} }), Return(Return { expr: Some(Var(Var { name: "y", deco: {"lineno": Int(6), "type": Ty(Int), "scope": Int(1), "offset": Int(1)} })), deco: {"lineno": Int(6)} })], deco: {"lineno": Int(3), "type": Ty(Int), "label": Str("f_uniqstr3"), "scope": Int(1), "var_cnt": Int(2)} }], body: [Assign(Assign { name: "x", expr: ArithOp(ArithOp { op: "+", left: FunCall(FunCall { name: "f", args: [Integer(Integer { value: 3, deco: {"lineno": Int(8), "type": Ty(Int)} })], deco: {"lineno": Int(8), "label": Str("f_uniqstr3"), "scope": Int(1), "var_cnt": Int(2), "type": Ty(Int)} }), right: Integer(Integer { value: 1, deco: {"lineno": Int(8), "type": Ty(Int)} }), deco: {"lineno": Int(8), "type": Ty(Int)} }), deco: {"lineno": Int(8), "type": Ty(Int), "scope": Int(0), "offset": Int(0)} }), Print(Print { expr: Var(Var { name: "x", deco: {"lineno": Int(9), "type": Ty(Int), "scope": Int(0), "offset": Int(0)} }), newline: true, deco: {"lineno": Int(9)} })], deco: {"label": Str("main_uniqstr4"), "type": Ty(Void), "lineno": Int(1), "scope": Int(0), "var_cnt": Int(1), "strings": Set([]), "scope_cnt": Int(2)} }
*/

