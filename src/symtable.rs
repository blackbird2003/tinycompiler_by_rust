use std::collections::HashMap;
use crate::syntree::{Deco, DecoValue, Type};

pub type ScopeId = usize;
pub type Offset  = usize;

/// 函数签名：(name, arg types...)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunSignature {
    pub name: String,
    pub arg_types: Vec<Type>,
}

#[derive(Debug)]
pub struct SymbolTable {
    /// 变量符号表栈：每一层是 name -> Deco
    variables: Vec<HashMap<String, Deco>>,

    /// 函数符号表栈：每一层是 signature -> Deco
    functions: Vec<HashMap<FunSignature, Deco>>,

    /// 当前所在函数的 deco（用于 return 检查 + var offset 分配）
    ret_stack: Vec<Deco>,

    /// 全局 scope 计数器（display index）
    scope_cnt: ScopeId,
}


impl SymbolTable {
    pub fn new() -> Self {
        Self {
            variables: vec![HashMap::new()],
            functions: vec![HashMap::new()],
            ret_stack: Vec::new(),
            scope_cnt: 0,
        }
    }

    pub fn add_fun(
        &mut self,
        name: &str,
        arg_types: &[Type],
        mut deco: Deco,
    ) -> Result<(), String> {
        let sig = FunSignature {
            name: name.to_string(),
            arg_types: arg_types.to_vec(),
        };

        let cur = self.functions.last_mut().unwrap();
        if cur.contains_key(&sig) {
            return Err(format!(
                "Double declaration of function {}{:?}",
                name, arg_types
            ));
        }

        // 写 scope id
        deco.insert(
            "scope".into(),
            DecoValue::Int(self.scope_cnt as i64),
        );
        self.scope_cnt += 1;

        cur.insert(sig, deco);
        Ok(())
    }

    pub fn add_var(
        &mut self,
        name: &str,
        deco: &mut Deco,
    ) -> Result<(), String> {
        let cur = self.variables.last_mut().unwrap();
        if cur.contains_key(name) {
            return Err(format!(
                "Double declaration of variable {}",
                name
            ));
        }

        // 当前函数
        let fun_deco = self.ret_stack.last_mut()
            .expect("add_var called outside function");

        let scope = match fun_deco.get("scope") {
            Some(DecoValue::Int(v)) => *v as usize,
            _ => panic!("function deco has no scope"),
        };

        let var_cnt = match fun_deco.get("var_cnt") {
            Some(DecoValue::Int(v)) => *v as usize,
            _ => 0,
        };

        // 写回变量 deco
        deco.insert("scope".into(), DecoValue::Int(scope as i64));
        deco.insert("offset".into(), DecoValue::Int(var_cnt as i64));

        // var_cnt++
        fun_deco.insert(
            "var_cnt".into(),
            DecoValue::Int((var_cnt + 1) as i64),
        );

        cur.insert(name.to_string(), deco.clone());
        Ok(())
    }

    pub fn find_var(&self, name: &str) -> Result<Deco, String> {
        for scope in self.variables.iter().rev() {
            if let Some(deco) = scope.get(name) {
                return Ok(deco.clone());
            }
        }
        Err(format!("No declaration for variable {}", name))
    }

    pub fn find_fun(
        &self,
        name: &str,
        arg_types: &[Type],
    ) -> Result<Deco, String> {
        let sig = FunSignature {
            name: name.to_string(),
            arg_types: arg_types.to_vec(),
        };

        for scope in self.functions.iter().rev() {
            if let Some(deco) = scope.get(&sig) {
                return Ok(deco.clone());
            }
        }

        Err(format!(
            "No declaration for function {}{:?}",
            name, arg_types
        ))
    }

    pub fn push_scope(&mut self, fun_deco: Deco) {
        self.variables.push(HashMap::new());
        self.functions.push(HashMap::new());

        let mut fun_deco = fun_deco;
        fun_deco.insert("var_cnt".into(), DecoValue::Int(0));
        self.ret_stack.push(fun_deco);
    }

    pub fn pop_scope(&mut self) {
        self.variables.pop();
        self.functions.pop();
        self.ret_stack.pop();
    }
}
