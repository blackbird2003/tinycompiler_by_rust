use std::collections::HashMap;
use crate::syntree::{Deco, DecoValue, Type};

pub type ScopeId = usize;
pub type Offset  = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunSignature {
    pub name: String,
    pub arg_types: Vec<Type>,
}

#[derive(Debug)]
pub struct SymbolTable {
    variables: Vec<HashMap<String, Deco>>,
    functions: Vec<HashMap<FunSignature, Deco>>,
    ret_stack: Vec<Deco>,
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

    /* ---------- functions ---------- */

    pub fn add_fun(
        &mut self,
        name: &str,
        arg_types: &[Type],
        deco: &mut Deco,
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

        deco.insert(
            "scope".into(),
            DecoValue::Int(self.scope_cnt as i64),
        );
        self.scope_cnt += 1;

        cur.insert(sig, deco.clone());
        Ok(())
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

    /* ---------- variables ---------- */

    pub fn add_var(
        &mut self,
        name: &str,
        deco: &mut Deco,
    ) -> Result<(), String> {
        let cur = self.variables.last_mut().unwrap();
        if cur.contains_key(name) {
            return Err(format!("Double declaration of variable {}", name));
        }

        let fun_deco = self
            .ret_stack
            .last_mut()
            .expect("add_var called outside function");

        let scope = match fun_deco.get("scope") {
            Some(DecoValue::Int(v)) => *v as usize,
            _ => panic!("function deco has no scope"),
        };

        let var_cnt = match fun_deco.get("var_cnt") {
            Some(DecoValue::Int(v)) => *v as usize,
            _ => 0,
        };

        deco.insert("scope".into(), DecoValue::Int(scope as i64));
        deco.insert("offset".into(), DecoValue::Int(var_cnt as i64));

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

    /* ---------- scope management ---------- */

    pub fn push_scope(&mut self, fun_deco: &mut Deco) {
        self.variables.push(HashMap::new());
        self.functions.push(HashMap::new());

        fun_deco.insert("var_cnt".into(), DecoValue::Int(0));
        self.ret_stack.push(fun_deco.clone());
    }

    pub fn pop_scope(&mut self) {
        self.variables.pop();
        self.functions.pop();
        self.ret_stack.pop();
    }

    /* ---------- query helpers ---------- */

    pub fn current_return_type(&self) -> Option<Type> {
        self.ret_stack.last().and_then(|d| match d.get("type") {
            Some(DecoValue::Ty(t)) => Some(*t),
            _ => None,
        })
    }

    pub fn scope_cnt(&self) -> ScopeId {
        self.scope_cnt
    }
}
