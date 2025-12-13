use std::collections::HashMap;
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
struct Token {
    token_type: String,
    value: String,
}

impl Token {
    fn __init__(t: &str, v: &str) -> Self {
        Self {
            token_type: t.to_string(),
            value: v.to_string(),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 对应 Python: f'{self.type}({self.value})'
        write!(f, "{}({})", self.token_type, self.value)
    }
}

fn tokenize(text: &str) -> Result<Vec<Token>, String> {
    let keywords: HashMap<&str, &str> = HashMap::from([
        ("true", "BOOLEAN"),
        ("false", "BOOLEAN"),
        ("print", "PRINT"),
        ("println", "PRINT"),
        ("int", "TYPE"),
        ("bool", "TYPE"),
        ("if", "IF"),
        ("else", "ELSE"),
        ("while", "WHILE"),
        ("return", "RETURN"),
    ]);

    let double_char: HashMap<&str, &str> = HashMap::from([
        ("==", "COMP"),
        ("<=", "COMP"),
        (">=", "COMP"),
        ("!=", "COMP"),
        ("&&", "AND"),
        ("||", "OR"),
    ]);

    let single_char: HashMap<char, &str> = HashMap::from([
        ('=', "ASSIGN"),
        ('<', "COMP"),
        ('>', "COMP"),
        ('!', "NOT"),
        ('+', "PLUS"),
        ('-', "MINUS"),
        ('/', "DIVIDE"),
        ('*', "TIMES"),
        ('%', "MOD"),
        ('(', "LPAREN"),
        (')', "RPAREN"),
        ('{', "BEGIN"),
        ('}', "END"),
        (';', "SEMICOLON"),
        (',', "COMMA"),
    ]);

    let chars: Vec<char> = text.chars().collect();
    let mut tokens = Vec::new();

    let mut idx: usize = 0;
    let mut state: i32 = 0;
    let mut accum = String::new();

    while idx < chars.len() {
        let sym1 = if idx < chars.len() { chars[idx] } else { ' ' };
        let sym2 = if idx + 1 < chars.len() { chars[idx + 1] } else { ' ' };

        if state == 0 {
            if sym1 == '/' && sym2 == '/' {
                state = 1; // comment
            } else if sym1.is_ascii_digit() {
                state = 2;
                accum.push(sym1);
            } else if sym1 == '"' {
                state = 3;
            } else if sym1.is_ascii_alphabetic() || sym1 == '_' {
                state = 4;
                accum.push(sym1);
            } else {
                let two = [sym1, sym2].iter().collect::<String>();
                if let Some(tok_type) = double_char.get(two.as_str()) {
                    tokens.push(Token::__init__(tok_type, &two));
                    idx += 1;
                } else if let Some(tok_type) = single_char.get(&sym1) {
                    tokens.push(Token::__init__(tok_type, &sym1.to_string()));
                } else if !matches!(sym1, '\r' | '\t' | ' ' | '\n') {
                    return Err(format!(
                        "Lexical error: illegal character \"{}\"",
                        sym1
                    ));
                }
            }
        } else if state == 2 {
            if sym1.is_ascii_digit() {
                accum.push(sym1);
            } else {
                tokens.push(Token::__init__("INTEGER", &accum));
                idx = idx.saturating_sub(1);
                state = 0;
                accum.clear();
            }
        } else if state == 3 {
            let escaped = accum.chars().last() == Some('\\');
            if sym1 != '"' || (!accum.is_empty() && escaped) {
                accum.push(sym1);
            } else {
                tokens.push(Token::__init__("STRING", &accum));
                state = 0;
                accum.clear();
            }
        } else if state == 4 {
            if sym1.is_ascii_alphanumeric() || sym1 == '_' {
                accum.push(sym1);
            } else {
                if let Some(tok_type) = keywords.get(accum.as_str()) {
                    tokens.push(Token::__init__(tok_type, &accum));
                } else {
                    tokens.push(Token::__init__("ID", &accum));
                }
                idx = idx.saturating_sub(1);
                state = 0;
                accum.clear();
            }
        }

        if sym1 == '\n' && state == 1 {
            state = 0;
            accum.clear();
        }

        idx += 1;
    }

    if state != 0 {
        return Err("Lexical error: unexpected EOF".to_string());
    }

    Ok(tokens)
}

// fn main() -> Result<(), String> {
//     let input = r#"0 // this is a comment to ignore by the lexer
// "this is a string with an escaped quotation mark \\" and double slash // that is not considered as a comment"
// 1337
// _identifier
// while
// truefalse
// 5*3 = -6
// 2 && 2 == 5;
// "#;

//     let tokens = tokenize(input)?;
//     println!("{:?}", tokens);
//     Ok(())
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_complex_input() {
        let input = r#"0 // this is a comment to ignore by the lexer
        "this is a string with an escaped quotation mark \\" and double slash // that is not considered as a comment"
        1337
        _identifier
        while
        truefalse
        5*3 = -6
        2 && 2 == 5;
        "#;

        let tokens = tokenize(input).expect("tokenize failed");
        println!("{:#?}", tokens);
        // // 最基本的断言：至少生成了一些 token
        // assert!(!tokens.is_empty());

        let expected = vec![
            Token::__init__("INTEGER", "0"),
            Token::__init__(
                "STRING",
                r#"this is a string with an escaped quotation mark \\" and double slash // that is not considered as a comment"#,
            ),
            Token::__init__("INTEGER", "1337"),
            Token::__init__("ID", "_identifier"),
            Token::__init__("WHILE", "while"),
            Token::__init__("ID", "truefalse"),
            Token::__init__("INTEGER", "5"),
            Token::__init__("TIMES", "*"),
            Token::__init__("INTEGER", "3"),
            Token::__init__("ASSIGN", "="),
            Token::__init__("MINUS", "-"),
            Token::__init__("INTEGER", "6"),
            Token::__init__("INTEGER", "2"),
            Token::__init__("AND", "&&"),
            Token::__init__("INTEGER", "2"),
            Token::__init__("COMP", "=="),
            Token::__init__("INTEGER", "5"),
            Token::__init__("SEMICOLON", ";"),
        ];

        assert_eq!(tokens, expected);
    }
}