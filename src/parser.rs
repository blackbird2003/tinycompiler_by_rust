
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;

use crate::lexer::Token;
use crate::syntree::{
    deco_i, deco_merge, deco_t, ArithOp, Assign, Boolean, Expr, FunCall, Function, IfThenElse,
    Integer, LogicOp, Print, Return, Stmt, StringLit, Type, Var, While,
};

/// A parse error (syntax error) produced by the Earley parser.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ParseError {}

/// Internal grammar rule (lhs -> rhs0 rhs1 ...).
#[derive(Debug, Clone)]
struct Rule {
    lhs: &'static str,
    rhs: &'static [&'static str],
}

/// The grammar, kept identical to the Python reference implementation.
/// See `parser.py` for the authoritative list/order. (Rule indices must match.)
const GRAMMAR: &[Rule] = &[
    Rule { lhs: "fun", rhs: &["fun_type", "ID", "LPAREN", "param_list", "RPAREN", "BEGIN", "var_list", "fun_list", "statement_list", "END"] },
    Rule { lhs: "var", rhs: &["TYPE", "ID"] },
    Rule { lhs: "param_list", rhs: &["var"] },
    Rule { lhs: "param_list", rhs: &[] },
    Rule { lhs: "param_list", rhs: &["param_list", "COMMA", "var"] },
    Rule { lhs: "fun_type", rhs: &["TYPE"] },
    Rule { lhs: "fun_type", rhs: &[] },
    Rule { lhs: "var_list", rhs: &["var_list", "var", "SEMICOLON"] },
    Rule { lhs: "var_list", rhs: &[] },
    Rule { lhs: "fun_list", rhs: &["fun_list", "fun"] },
    Rule { lhs: "fun_list", rhs: &[] },
    Rule { lhs: "statement_list", rhs: &["statement_list", "statement"] },
    Rule { lhs: "statement_list", rhs: &[] },
    Rule { lhs: "statement", rhs: &["ID", "LPAREN", "arg_list", "RPAREN", "SEMICOLON"] },
    Rule { lhs: "statement", rhs: &["ID", "ASSIGN", "expr", "SEMICOLON"] },
    Rule { lhs: "statement", rhs: &["RETURN", "expr", "SEMICOLON"] },
    Rule { lhs: "statement", rhs: &["RETURN", "SEMICOLON"] },
    Rule { lhs: "statement", rhs: &["PRINT", "expr", "SEMICOLON"] },
    Rule { lhs: "statement", rhs: &["IF", "expr", "BEGIN", "statement_list", "END", "else_statement"] },
    Rule { lhs: "else_statement", rhs: &["ELSE", "BEGIN", "statement_list", "END"] },
    Rule { lhs: "else_statement", rhs: &[] },
    Rule { lhs: "statement", rhs: &["WHILE", "expr", "BEGIN", "statement_list", "END"] },
    Rule { lhs: "arg_list", rhs: &["expr"] },
    Rule { lhs: "arg_list", rhs: &["arg_list", "COMMA", "expr"] },
    Rule { lhs: "arg_list", rhs: &[] },
    Rule { lhs: "expr", rhs: &["conjunction"] },
    Rule { lhs: "expr", rhs: &["expr", "OR", "conjunction"] },
    Rule { lhs: "expr", rhs: &["STRING"] },
    Rule { lhs: "conjunction", rhs: &["literal"] },
    Rule { lhs: "conjunction", rhs: &["conjunction", "AND", "literal"] },
    Rule { lhs: "literal", rhs: &["comparand"] },
    Rule { lhs: "literal", rhs: &["NOT", "comparand"] },
    Rule { lhs: "comparand", rhs: &["addend"] },
    Rule { lhs: "comparand", rhs: &["addend", "COMP", "addend"] },
    Rule { lhs: "addend", rhs: &["term"] },
    Rule { lhs: "addend", rhs: &["addend", "MINUS", "term"] },
    Rule { lhs: "addend", rhs: &["addend", "PLUS", "term"] },
    Rule { lhs: "term", rhs: &["factor"] },
    Rule { lhs: "term", rhs: &["term", "MOD", "factor"] },
    Rule { lhs: "term", rhs: &["term", "DIVIDE", "factor"] },
    Rule { lhs: "term", rhs: &["term", "TIMES", "factor"] },
    Rule { lhs: "factor", rhs: &["atom"] },
    Rule { lhs: "factor", rhs: &["PLUS", "atom"] },
    Rule { lhs: "factor", rhs: &["MINUS", "atom"] },
    Rule { lhs: "atom", rhs: &["BOOLEAN"] },
    Rule { lhs: "atom", rhs: &["INTEGER"] },
    Rule { lhs: "atom", rhs: &["ID", "LPAREN", "arg_list", "RPAREN"] },
    Rule { lhs: "atom", rhs: &["ID"] },
    Rule { lhs: "atom", rhs: &["LPAREN", "expr", "RPAREN"] },
];

/// A single Earley item (ParseState in the Python reference).
///
/// Equality / de-duplication matches Python: only (rule, dot, start) are compared,
/// `token_pos` and `prev` are ignored. fileciteturn1file0L4-L17
#[derive(Clone, Debug)]
struct ParseState {
    rule: usize,
    dot: usize,
    start: usize,

    /// How many tokens we have consumed up to the current dot position (used for AST recovery).
    token_pos: usize,

    /// Previous-state pointer used to reconstruct one parse path.
    prev: Option<usize>,
}

impl ParseState {
    fn key(&self) -> (usize, usize, usize) {
        (self.rule, self.dot, self.start)
    }

    fn next_symbol(&self) -> Option<&'static str> {
        let rhs = GRAMMAR[self.rule].rhs;
        rhs.get(self.dot).copied()
    }

    fn finished(&self) -> bool {
        self.dot >= GRAMMAR[self.rule].rhs.len()
    }
}

/// Semantic value used during AST building (a Python "stack" element).
#[derive(Clone, Debug)]
enum SemVal {
    Tok(Token),
    Ty(Type),

    Var(Var),
    Vars(Vec<Var>),

    Fun(Function),
    Funs(Vec<Function>),

    Stmt(Stmt),
    Stmts(Vec<Stmt>),

    Expr(Expr),
    Exprs(Vec<Expr>),
}

fn tok(v: &SemVal) -> &Token {
    match v {
        SemVal::Tok(t) => t,
        _ => panic!("expected Token"),
    }
}

fn take_tok(v: SemVal) -> Token {
    match v {
        SemVal::Tok(t) => t,
        _ => panic!("expected Token"),
    }
}

fn take_ty(v: SemVal) -> Type {
    match v {
        SemVal::Ty(t) => t,
        _ => panic!("expected Type"),
    }
}

fn take_var(v: SemVal) -> Var {
    match v {
        SemVal::Var(x) => x,
        _ => panic!("expected Var"),
    }
}

fn take_vars(v: SemVal) -> Vec<Var> {
    match v {
        SemVal::Vars(xs) => xs,
        _ => panic!("expected Vec<Var>"),
    }
}

fn take_fun(v: SemVal) -> Function {
    match v {
        SemVal::Fun(x) => x,
        _ => panic!("expected Function"),
    }
}

fn take_funs(v: SemVal) -> Vec<Function> {
    match v {
        SemVal::Funs(xs) => xs,
        _ => panic!("expected Vec<Function>"),
    }
}

fn take_stmt(v: SemVal) -> Stmt {
    match v {
        SemVal::Stmt(x) => x,
        _ => panic!("expected Stmt"),
    }
}

fn take_stmts(v: SemVal) -> Vec<Stmt> {
    match v {
        SemVal::Stmts(xs) => xs,
        _ => panic!("expected Vec<Stmt>"),
    }
}

fn take_expr(v: SemVal) -> Expr {
    match v {
        SemVal::Expr(x) => x,
        _ => panic!("expected Expr"),
    }
}

fn take_exprs(v: SemVal) -> Vec<Expr> {
    match v {
        SemVal::Exprs(xs) => xs,
        _ => panic!("expected Vec<Expr>"),
    }
}

/// Wend parser (Earley).
///
/// Reference algorithm and rule order follow `parser.py` and the README. fileciteturn1file6L1-L46 fileciteturn1file1L18-L47
pub struct WendParser {
    nonterminals: HashSet<&'static str>,
}

impl WendParser {
    pub fn new() -> Self {
        let mut nts = HashSet::new();
        for r in GRAMMAR {
            nts.insert(r.lhs);
        }
        Self { nonterminals: nts }
    }

    fn is_nonterminal(&self, sym: &str) -> bool {
        self.nonterminals.contains(sym)
    }

    /// Parse a token sequence into a syntax tree (Function).
    pub fn parse<I>(&self, tokens: I) -> Result<Function, ParseError>
    where
        I: IntoIterator<Item = Token>,
    {
        let toks: Vec<Token> = tokens.into_iter().collect();
        let (arena, _charts, goal_state) = self.recognize_with_arena(&toks)?;
        self.build_syntree_from_arena(&toks, &arena, goal_state)
    }


    fn append_state(
        &self,
        arena: &mut Vec<ParseState>,
        charts: &mut Vec<Vec<usize>>,
        keys: &mut Vec<HashSet<(usize, usize, usize)>>,
        chart_index: usize,
        state: ParseState,
    ) {
        if charts.len() <= chart_index {
            charts.resize_with(chart_index + 1, Vec::new);
            keys.resize_with(chart_index + 1, HashSet::new);
        }
        let k = state.key();
        if keys[chart_index].contains(&k) {
            return;
        }
        let idx = arena.len();
        arena.push(state);
        charts[chart_index].push(idx);
        keys[chart_index].insert(k);
    }

    /// Recover one parse path and build the syntax tree (Function).
    fn build_syntree_from_arena(
        &self,
        tokens: &[Token],
        arena: &[ParseState],
        goal_state: usize,
    ) -> Result<Function, ParseError> {
        let mut production: Vec<usize> = Vec::new();
        let mut cur = Some(goal_state);
        while let Some(idx) = cur {
            let st = &arena[idx];
            if st.finished() {
                production.push(idx);
            }
            cur = st.prev;
        }

        let mut stack: Vec<SemVal> = Vec::new();
        let mut token_cursor: usize = 0;

        for &rid in production.iter().rev() {
            let st = &arena[rid];

            for t in tokens[token_cursor..st.token_pos].iter().cloned() {
                stack.push(SemVal::Tok(t));
            }
            token_cursor = st.token_pos;

            let chomp = GRAMMAR[st.rule].rhs.len();
            let mut chew: Vec<SemVal> = Vec::new();
            if chomp > 0 {
                let start = stack.len() - chomp;
                chew.extend(stack.drain(start..));
            }

            let node = build_rule(st.rule, chew);
            stack.push(node);
        }

        match stack.pop() {
            Some(SemVal::Fun(f)) => Ok(f),
            other => Err(ParseError::new(format!(
                "Internal parser error: expected Function at end, got {:?}",
                other
            ))),
        }
    }

    /// Earley recognizer: returns the state arena, the charts, and an arena index of a completed start rule.
    fn recognize_with_arena(
        &self,
        tokens: &[Token],
    ) -> Result<(Vec<ParseState>, Vec<Vec<usize>>, usize), ParseError> {
        let mut states: Vec<ParseState> = Vec::new();
        let mut charts: Vec<Vec<usize>> = vec![Vec::new()];

        let s0 = ParseState {
            rule: 0,
            dot: 0,
            start: 0,
            token_pos: 0,
            prev: None,
        };
        states.push(s0);
        charts[0].push(0);

        let mut chart_keys: Vec<HashSet<(usize, usize, usize)>> = vec![HashSet::new()];
        chart_keys[0].insert((0, 0, 0));

        let mut pos: usize = 0;
        loop {
            let cur_tok: Option<&Token> = tokens.get(pos);

            if charts.len() == pos {
                charts.push(Vec::new());
                chart_keys.push(HashSet::new());
            }

            let mut i = 0;
            while i < charts[pos].len() {
                let st_idx = charts[pos][i];
                let st = states[st_idx].clone();

                match st.next_symbol() {
                    None => {
                        let completed_lhs = GRAMMAR[st.rule].lhs;
                        let start_chart = st.start;
                        let origin_items = charts[start_chart].clone();
                        for &item_idx in origin_items.iter() {
                            let item = &states[item_idx];
                            if item.next_symbol() == Some(completed_lhs) {
                                let next = ParseState {
                                    rule: item.rule,
                                    dot: item.dot + 1,
                                    start: item.start,
                                    token_pos: pos,
                                    prev: Some(st_idx),
                                };
                                self.append_state(
                                    &mut states,
                                    &mut charts,
                                    &mut chart_keys,
                                    pos,
                                    next,
                                );
                            }
                        }
                    }
                    Some(sym) if !self.is_nonterminal(sym) => {
                        if let Some(t) = cur_tok {
                            if sym == t.token_type {
                                let next = ParseState {
                                    rule: st.rule,
                                    dot: st.dot + 1,
                                    start: st.start,
                                    token_pos: pos + 1,
                                    prev: Some(st_idx),
                                };
                                self.append_state(
                                    &mut states,
                                    &mut charts,
                                    &mut chart_keys,
                                    pos + 1,
                                    next,
                                );
                            }
                        }
                    }
                    Some(sym) => {
                        for (idx, r) in GRAMMAR.iter().enumerate() {
                            if r.lhs == sym {
                                let next = ParseState {
                                    rule: idx,
                                    dot: 0,
                                    start: pos,
                                    token_pos: pos,
                                    prev: Some(st_idx),
                                };
                                self.append_state(
                                    &mut states,
                                    &mut charts,
                                    &mut chart_keys,
                                    pos,
                                    next,
                                );
                            }
                        }
                    }
                }

                i += 1;
            }

            if cur_tok.is_some() && charts.len() == pos + 1 {
                let t = cur_tok.unwrap();
                return Err(ParseError::new(format!(
                    "Syntax error at line {}, token={}",
                    t.lineno, t.token_type
                )));
            }

            if pos >= tokens.len() {
                break;
            }
            pos += 1;
        }

        let end_pos = tokens.len();
        let goal_dot = GRAMMAR[0].rhs.len();
        for &st_idx in charts[end_pos].iter() {
            let st = &states[st_idx];
            if st.rule == 0 && st.dot == goal_dot && st.start == 0 {
                return Ok((states, charts, st_idx));
            }
        }
        Err(ParseError::new("Syntax error: unexpected EOF"))
    }
}

/// Build an AST node for one grammar rule.
///
/// This mirrors the per-rule lambda constructors in `parser.py`. fileciteturn1file0L19-L38 fileciteturn1file2L1-L20
fn build_rule(rule_idx: usize, mut p: Vec<SemVal>) -> SemVal {
    // p is "chew": RHS semantic values in order.
    match rule_idx {
        // fun -> fun_type ID ( param_list ) { var_list fun_list statement_list }
        0 => {
            let fun_type = take_ty(p.remove(0));
            let id = take_tok(p.remove(0));
            let _lparen = p.remove(0);
            let params = take_vars(p.remove(0));
            let _rparen = p.remove(0);
            let _begin = p.remove(0);
            let vars = take_vars(p.remove(0));
            let funs = take_funs(p.remove(0));
            let body = take_stmts(p.remove(0));
            let _end = p.remove(0);

            let deco = deco_merge(deco_t("type", fun_type), deco_i("lineno", id.lineno as i64));
            SemVal::Fun(Function::new(id.value, params, vars, funs, body, deco))
        }

        // var -> TYPE ID
        1 => {
            let ty_tok = take_tok(p.remove(0));
            let id = take_tok(p.remove(0));
            let ty = Type::from_type_keyword(&ty_tok.value);
            let deco = deco_merge(deco_t("type", ty), deco_i("lineno", ty_tok.lineno as i64));
            SemVal::Var(Var::new(id.value, deco))
        }

        // param_list -> var
        2 => SemVal::Vars(vec![take_var(p.remove(0))]),
        // param_list -> ε
        3 => SemVal::Vars(vec![]),
        // param_list -> param_list , var
        4 => {
            let mut xs = take_vars(p.remove(0));
            let _comma = p.remove(0);
            xs.push(take_var(p.remove(0)));
            SemVal::Vars(xs)
        }

        // fun_type -> TYPE
        5 => {
            let ty_tok = take_tok(p.remove(0));
            SemVal::Ty(Type::from_type_keyword(&ty_tok.value))
        }
        // fun_type -> ε
        6 => SemVal::Ty(Type::Void),

        // var_list -> var_list var ;
        7 => {
            let mut xs = take_vars(p.remove(0));
            xs.push(take_var(p.remove(0)));
            let _semi = p.remove(0);
            SemVal::Vars(xs)
        }
        // var_list -> ε
        8 => SemVal::Vars(vec![]),

        // fun_list -> fun_list fun
        9 => {
            let mut xs = take_funs(p.remove(0));
            xs.push(take_fun(p.remove(0)));
            SemVal::Funs(xs)
        }
        // fun_list -> ε
        10 => SemVal::Funs(vec![]),

        // statement_list -> statement_list statement
        11 => {
            let mut xs = take_stmts(p.remove(0));
            xs.push(take_stmt(p.remove(0)));
            SemVal::Stmts(xs)
        }
        // statement_list -> ε
        12 => SemVal::Stmts(vec![]),

        // statement -> ID ( arg_list ) ;
        13 => {
            let id = take_tok(p.remove(0));
            let _lparen = p.remove(0);
            let args = take_exprs(p.remove(0));
            let _rparen = p.remove(0);
            let _semi = p.remove(0);

            let deco = deco_i("lineno", id.lineno as i64);
            let call = FunCall::new(id.value, args, deco);
            SemVal::Stmt(Stmt::FunCall(call))
        }

        // statement -> ID = expr ;
        14 => {
            let id = take_tok(p.remove(0));
            let _assign = p.remove(0);
            let expr = take_expr(p.remove(0));
            let _semi = p.remove(0);
            let deco = deco_i("lineno", id.lineno as i64);
            SemVal::Stmt(Stmt::Assign(Assign::new(id.value, expr, deco)))
        }

        // statement -> return expr ;
        15 => {
            let ret = take_tok(p.remove(0));
            let expr = take_expr(p.remove(0));
            let _semi = p.remove(0);
            let deco = deco_i("lineno", ret.lineno as i64);
            SemVal::Stmt(Stmt::Return(Return::new(Some(expr), deco)))
        }

        // statement -> return ;
        16 => {
            let ret = take_tok(p.remove(0));
            let _semi = p.remove(0);
            let deco = deco_i("lineno", ret.lineno as i64);
            SemVal::Stmt(Stmt::Return(Return::new(None, deco)))
        }

        // statement -> print expr ;
        17 => {
            let print_tok = take_tok(p.remove(0));
            let expr = take_expr(p.remove(0));
            let _semi = p.remove(0);
            let newline = print_tok.value == "println";
            let deco = deco_i("lineno", print_tok.lineno as i64);
            SemVal::Stmt(Stmt::Print(Print::new(expr, newline, deco)))
        }

        // statement -> if expr { statement_list } else_statement
        18 => {
            let if_tok = take_tok(p.remove(0));
            let expr = take_expr(p.remove(0));
            let _begin = p.remove(0);
            let ibody = take_stmts(p.remove(0));
            let _end = p.remove(0);
            let ebody = take_stmts(p.remove(0));
            let deco = deco_i("lineno", if_tok.lineno as i64);
            SemVal::Stmt(Stmt::IfThenElse(IfThenElse::new(expr, ibody, ebody, deco)))
        }

        // else_statement -> else { statement_list }
        19 => {
            let _else_tok = p.remove(0);
            let _begin = p.remove(0);
            let body = take_stmts(p.remove(0));
            let _end = p.remove(0);
            SemVal::Stmts(body)
        }
        // else_statement -> ε
        20 => SemVal::Stmts(vec![]),

        // statement -> while expr { statement_list }
        21 => {
            let while_tok = take_tok(p.remove(0));
            let expr = take_expr(p.remove(0));
            let _begin = p.remove(0);
            let body = take_stmts(p.remove(0));
            let _end = p.remove(0);
            let deco = deco_i("lineno", while_tok.lineno as i64);
            SemVal::Stmt(Stmt::While(While::new(expr, body, deco)))
        }

        // arg_list -> expr
        22 => SemVal::Exprs(vec![take_expr(p.remove(0))]),
        // arg_list -> arg_list , expr
        23 => {
            let mut xs = take_exprs(p.remove(0));
            let _comma = p.remove(0);
            xs.push(take_expr(p.remove(0)));
            SemVal::Exprs(xs)
        }
        // arg_list -> ε
        24 => SemVal::Exprs(vec![]),

        // expr -> conjunction
        25 => SemVal::Expr(take_expr(p.remove(0))),
        // expr -> expr OR conjunction
        26 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::LogicOp(LogicOp::new(op.value, left, right, deco)))
        }
        // expr -> STRING
        27 => {
            let t = take_tok(p.remove(0));
            let deco = deco_i("lineno", t.lineno as i64);
            SemVal::Expr(Expr::StringLit(StringLit::new(t.value, deco)))
        }

        // conjunction -> literal
        28 => SemVal::Expr(take_expr(p.remove(0))),
        // conjunction -> conjunction AND literal
        29 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::LogicOp(LogicOp::new(op.value, left, right, deco)))
        }

        // literal -> comparand
        30 => SemVal::Expr(take_expr(p.remove(0))),
        // literal -> NOT comparand
        31 => {
            let not_tok = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", not_tok.lineno as i64);
            let left = Expr::Boolean(Boolean::new(false, HashMap::new()));
            SemVal::Expr(Expr::LogicOp(LogicOp::new("==", left, right, deco)))
        }

        // comparand -> addend
        32 => SemVal::Expr(take_expr(p.remove(0))),
        // comparand -> addend COMP addend
        33 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::LogicOp(LogicOp::new(op.value, left, right, deco)))
        }

        // addend -> term
        34 => SemVal::Expr(take_expr(p.remove(0))),
        // addend -> addend - term
        35 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::ArithOp(ArithOp::new(op.value, left, right, deco)))
        }
        // addend -> addend + term
        36 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::ArithOp(ArithOp::new(op.value, left, right, deco)))
        }

        // term -> factor
        37 => SemVal::Expr(take_expr(p.remove(0))),
        // term -> term % factor
        38 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::ArithOp(ArithOp::new(op.value, left, right, deco)))
        }
        // term -> term / factor
        39 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::ArithOp(ArithOp::new(op.value, left, right, deco)))
        }
        // term -> term * factor
        40 => {
            let left = take_expr(p.remove(0));
            let op = take_tok(p.remove(0));
            let right = take_expr(p.remove(0));
            let deco = deco_i("lineno", op.lineno as i64);
            SemVal::Expr(Expr::ArithOp(ArithOp::new(op.value, left, right, deco)))
        }

        // factor -> atom
        41 => SemVal::Expr(take_expr(p.remove(0))),
        // factor -> + atom
        42 => {
            let _plus = p.remove(0);
            SemVal::Expr(take_expr(p.remove(0)))
        }
        // factor -> - atom
        43 => {
            let minus = take_tok(p.remove(0));
            let atom = take_expr(p.remove(0));
            let deco = deco_i("lineno", minus.lineno as i64);
            let zero = Expr::Integer(Integer::new(0, HashMap::new()));
            SemVal::Expr(Expr::ArithOp(ArithOp::new("-", zero, atom, deco)))
        }

        // atom -> BOOLEAN
        44 => {
            let t = take_tok(p.remove(0));
            let v = t.value == "true";
            let deco = deco_i("lineno", t.lineno as i64);
            SemVal::Expr(Expr::Boolean(Boolean::new(v, deco)))
        }
        // atom -> INTEGER
        45 => {
            let t = take_tok(p.remove(0));
            let n: i64 = t
                .value
                .parse()
                .expect("lexer should only emit valid INTEGER tokens");
            let deco = deco_i("lineno", t.lineno as i64);
            SemVal::Expr(Expr::Integer(Integer::new(n, deco)))
        }
        // atom -> ID ( arg_list )
        46 => {
            let id = take_tok(p.remove(0));
            let _lparen = p.remove(0);
            let args = take_exprs(p.remove(0));
            let _rparen = p.remove(0);
            let deco = deco_i("lineno", id.lineno as i64);
            SemVal::Expr(Expr::FunCall(FunCall::new(id.value, args, deco)))
        }
        // atom -> ID
        47 => {
            let id = take_tok(p.remove(0));
            let deco = deco_i("lineno", id.lineno as i64);
            SemVal::Expr(Expr::Var(Var::new(id.value, deco)))
        }
        // atom -> ( expr )
        48 => {
            let _lparen = p.remove(0);
            let e = take_expr(p.remove(0));
            let _rparen = p.remove(0);
            SemVal::Expr(e)
        }

        _ => panic!("unknown rule index {}", rule_idx),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::WendLexer;

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
            // x = 1;
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

        println!("{:?}", ast);
    }
}
