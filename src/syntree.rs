use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

struct LabelFactory;

// 在代码生成阶段使用，用于生成唯一标签
impl LabelFactory {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn cur_label() -> String {
        format!("uniqstr{}", Self::COUNTER.load(Ordering::SeqCst))
    }

    fn new_label() -> String {
        let v = Self::COUNTER.fetch_add(1, Ordering::SeqCst) + 1;
        format!("uniqstr{}", v)
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Type {
    VOID   = 0,
    INT    = 1,
    BOOL   = 2,
    STRING = 3,
}


// 用于存储各种装饰信息
type Deco = HashMap<String, DecoValue>;
#[derive(Clone, Debug)]
enum DecoValue {
    Int(i32),
    Bool(bool),
    Str(String),
    Type(Type),
}


// 函数
#[derive(Debug)]
struct Function {
    name: String,                         // function name
    args: Vec<(String, Type)>,             // arguments
    var:  Vec<(String, Type)>,             // local variables
    fun:  Vec<Function>,                   // nested functions
    body: Vec<Stmt>,                       // body statements
    deco: Deco,
}

impl Function {
    fn new(
        name: String,
        args: Vec<(String, Type)>,
        var: Vec<(String, Type)>,
        fun: Vec<Function>,
        body: Vec<Stmt>,
        mut deco: Deco,
    ) -> Self {
        deco.insert(
            "label".to_string(),
            DecoValue::Str(format!("{}_{}", name, LabelFactory::new_label())),
        );
        Self { name, args, var, fun, body, deco }
    }
}


// 语句
#[derive(Debug)]
enum Stmt {
    Print(Print),
    Return(Return),
    Assign(Assign),
    While(While),
    IfThenElse(IfThenElse),
    FunCall(FunCall),
}

