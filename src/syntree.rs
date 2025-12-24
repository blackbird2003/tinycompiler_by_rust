use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::Mutex;

// -----------------------------
// LabelFactory (Python: class LabelFactory)
// -----------------------------

static LABEL_COUNTER: Mutex<usize> = Mutex::new(0);

/// A Python-like label factory.
///
/// Python version:
/// - cur_label() -> "uniqstr{counter}"
/// - new_label() increments then returns "uniqstr{counter}"
///
/// We keep the same behavior to minimize downstream diffs.
pub struct LabelFactory;

impl LabelFactory {
    pub fn cur_label() -> String {
        let c = *LABEL_COUNTER.lock().unwrap();
        format!("uniqstr{}", c)
    }

    pub fn new_label() -> String {
        let mut c = LABEL_COUNTER.lock().unwrap();
        *c += 1;
        format!("uniqstr{}", *c)
    }

    /// Helpful for tests / deterministic runs.
    pub fn reset() {
        let mut c = LABEL_COUNTER.lock().unwrap();
        *c = 0;
    }
}

// -----------------------------
// Type (Python: class Type)
// -----------------------------

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Void,
    Int,
    Bool,
    String,
}

impl Type {
    pub fn from_type_keyword(kw: &str) -> Self {
        match kw {
            "int" => Type::Int,
            "bool" => Type::Bool,
            _ => Type::Void,
        }
    }
}

// -----------------------------
// Decoration map (Python: node.deco dict)
// -----------------------------

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DecoValue {
    Int(i64),
    Bool(bool),
    Str(String),
    Ty(Type),

    /// Used by analyzer: main.deco['strings'] = set([(label, value), ...])
    /// We keep a set for easy dedup.
    StringSet(HashSet<(String, String)>),

    /// Used occasionally for convenience; keep minimal.
    StrList(Vec<String>),
}

pub type Deco = HashMap<String, DecoValue>;

pub fn deco() -> Deco {
    HashMap::new()
}

pub fn deco_merge(mut base: Deco, extra: Deco) -> Deco {
    // Python: base | extra (extra overwrites)
    for (k, v) in extra {
        base.insert(k, v);
    }
    base
}

// Small helpers (keep call sites simple, like Python dict literals)

pub fn deco_i(key: &str, v: i64) -> Deco {
    let mut d = HashMap::new();
    d.insert(key.to_string(), DecoValue::Int(v));
    d
}

pub fn deco_b(key: &str, v: bool) -> Deco {
    let mut d = HashMap::new();
    d.insert(key.to_string(), DecoValue::Bool(v));
    d
}

pub fn deco_s(key: &str, v: impl Into<String>) -> Deco {
    let mut d = HashMap::new();
    d.insert(key.to_string(), DecoValue::Str(v.into()));
    d
}

pub fn deco_t(key: &str, v: Type) -> Deco {
    let mut d = HashMap::new();
    d.insert(key.to_string(), DecoValue::Ty(v));
    d
}

// -----------------------------
// AST nodes (Python: syntree.py)
// -----------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub args: Vec<Var>,
    pub var: Vec<Var>,
    pub fun: Vec<Function>,
    pub body: Vec<Stmt>,
    pub deco: Deco,
}

impl Function {
    /// Python:
    /// self.deco = deco | {'label' : name+'_'+LabelFactory.new_label()}
    pub fn new(
        name: impl Into<String>,
        args: Vec<Var>,
        var: Vec<Var>,
        fun: Vec<Function>,
        body: Vec<Stmt>,
        deco_in: Deco,
    ) -> Self {
        let name_s = name.into();
        let label = format!("{}_{}", name_s, LabelFactory::new_label());

        let deco = deco_merge(deco_in, deco_s("label", label));

        Self {
            name: name_s,
            args,
            var,
            fun,
            body,
            deco,
        }
    }
}

// ----- statements -----

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Print(Print),
    Return(Return),
    Assign(Assign),
    While(While),
    IfThenElse(IfThenElse),

    /// Python grammar: statement -> ID '(' arg_list ')' ';' => FunCall(...)
    /// Python AST: FunCall can be stmt or expr. Keep one type and reuse.
    FunCall(FunCall),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Print {
    pub expr: Expr,
    pub newline: bool,
    pub deco: Deco,
}

impl Print {
    pub fn new(expr: Expr, newline: bool, deco: Deco) -> Self {
        Self { expr, newline, deco }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Return {
    pub expr: Option<Expr>,
    pub deco: Deco,
}

impl Return {
    pub fn new(expr: Option<Expr>, deco: Deco) -> Self {
        Self { expr, deco }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assign {
    pub name: String,
    pub expr: Expr,
    pub deco: Deco,
}

impl Assign {
    pub fn new(name: impl Into<String>, expr: Expr, deco: Deco) -> Self {
        Self {
            name: name.into(),
            expr,
            deco,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct While {
    pub expr: Expr,
    pub body: Vec<Stmt>,
    pub deco: Deco,
}

impl While {
    pub fn new(expr: Expr, body: Vec<Stmt>, deco: Deco) -> Self {
        Self { expr, body, deco }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfThenElse {
    pub expr: Expr,
    pub ibody: Vec<Stmt>,
    pub ebody: Vec<Stmt>,
    pub deco: Deco,
}

impl IfThenElse {
    pub fn new(expr: Expr, ibody: Vec<Stmt>, ebody: Vec<Stmt>, deco: Deco) -> Self {
        Self {
            expr,
            ibody,
            ebody,
            deco,
        }
    }
}

// ----- expressions -----

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    ArithOp(ArithOp),
    LogicOp(LogicOp),
    Integer(Integer),
    Boolean(Boolean),
    StringLit(StringLit),
    Var(Var),
    FunCall(FunCall),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArithOp {
    pub op: String,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub deco: Deco,
}

impl ArithOp {
    /// Python: deco | {'type' : Type.INT}
    pub fn new(op: impl Into<String>, left: Expr, right: Expr, deco_in: Deco) -> Self {
        let deco = deco_merge(deco_in, deco_t("type", Type::Int));
        Self {
            op: op.into(),
            left: Box::new(left),
            right: Box::new(right),
            deco,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LogicOp {
    pub op: String,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub deco: Deco,
}

impl LogicOp {
    /// Python: deco | {'type' : Type.BOOL}
    pub fn new(op: impl Into<String>, left: Expr, right: Expr, deco_in: Deco) -> Self {
        let deco = deco_merge(deco_in, deco_t("type", Type::Bool));
        Self {
            op: op.into(),
            left: Box::new(left),
            right: Box::new(right),
            deco,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Integer {
    pub value: i64,
    pub deco: Deco,
}

impl Integer {
    /// Python: deco | {'type' : Type.INT}
    pub fn new(value: i64, deco_in: Deco) -> Self {
        let deco = deco_merge(deco_in, deco_t("type", Type::Int));
        Self { value, deco }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
    pub deco: Deco,
}

impl Boolean {
    /// Python: deco | {'type' : Type.BOOL}
    pub fn new(value: bool, deco_in: Deco) -> Self {
        let deco = deco_merge(deco_in, deco_t("type", Type::Bool));
        Self { value, deco }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringLit {
    pub value: String,
    pub deco: Deco,
}

impl StringLit {
    /// Python: deco | {'type' : Type.STRING, 'label' : LabelFactory.new_label() }
    pub fn new(value: impl Into<String>, deco_in: Deco) -> Self {
        let label = LabelFactory::new_label();
        let deco = deco_merge(
            deco_merge(deco_in, deco_t("type", Type::String)),
            deco_s("label", label),
        );
        Self {
            value: value.into(),
            deco,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    pub name: String,
    pub deco: Deco,
}

impl Var {
    pub fn new(name: impl Into<String>, deco: Deco) -> Self {
        Self {
            name: name.into(),
            deco,
        }
    }
}

/// Python: FunCall can appear as a statement or an expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunCall {
    pub name: String,
    pub args: Vec<Expr>,
    pub deco: Deco,
}

impl FunCall {
    pub fn new(name: impl Into<String>, args: Vec<Expr>, deco: Deco) -> Self {
        Self {
            name: name.into(),
            args,
            deco,
        }
    }
}

// -----------------------------
// (Optional) pretty-print helpers
// -----------------------------

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Type::Void => "void",
            Type::Int => "int",
            Type::Bool => "bool",
            Type::String => "string",
        };
        write!(f, "{}", s)
    }
}

/*
An example:
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
The output AST:
Function { name: "main", args: [], var: [Var { name: "x", deco: {"type": Ty(Int), "scope": Int(0), "lineno": Int(1), "offset": Int(0)} }], fun: [Function { name: "f", args: [Var { name: "x", deco: {"offset": Int(0), "scope": Int(2), "lineno": Int(2), "type": Ty(Int)} }], var: [Var { name: "y", deco: {"type": Ty(Int), "scope": Int(2), "lineno": Int(3), "offset": Int(1)} }], fun: [], body: [Assign(Assign { name: "y", expr: ArithOp(ArithOp { op: "+", left: Var(Var { name: "x", deco: {"lineno": Int(2), "type": Ty(Int), "scope": Int(2), "offset": Int(0)} }), right: ArithOp(ArithOp { op: "*", left: Integer(Integer { value: 1, deco: {"lineno": Int(4), "type": Ty(Int)} }), right: Integer(Integer { value: 2, deco: {"lineno": Int(4), "type": Ty(Int)} }), deco: {"type": Ty(Int), "lineno": Int(4)} }), deco: {"lineno": Int(4), "type": Ty(Int)} }), deco: {"scope": Int(2), "lineno": Int(3), "type": Ty(Int), "offset": Int(1)} }), Return(Return { expr: Some(Var(Var { name: "y", deco: {"lineno": Int(3), "type": Ty(Int), "scope": Int(2), "offset": Int(1)} })), deco: {"lineno": Int(5)} })], deco: {"lineno": Int(2), "label": Str("f_uniqstr3"), "type": Ty(Int), "scope": Int(2), "var_cnt": Int(2)} }], body: [Assign(Assign { name: "x", expr: ArithOp(ArithOp { op: "+", left: FunCall(FunCall { name: "f", args: [Integer(Integer { value: 3, deco: {"type": Ty(Int), "lineno": Int(7)} })], deco: {"lineno": Int(2), "scope": Int(2), "type": Ty(Int), "var_cnt": Int(2), "label": Str("f_uniqstr3")} }), right: Integer(Integer { value: 1, deco: {"lineno": Int(7), "type": Ty(Int)} }), deco: {"lineno": Int(7), "type": Ty(Int)} }), deco: {"lineno": Int(1), "type": Ty(Int), "offset": Int(0), "scope": Int(0)} }), Print(Print { expr: Var(Var { name: "x", deco: {"offset": Int(0), "scope": Int(0), "lineno": Int(1), "type": Ty(Int)} }), newline: true, deco: {"lineno": Int(8)} })], deco: {"var_cnt": Int(1), "type": Ty(Void), "strings": StringSet({}), "lineno": Int(0), "scope": Int(0), "scope_cnt": Int(3), "label": Str("main_uniqstr4")} }
*/

