use std::error::Error;
use std::fmt;

/// A token produced by the Wend lexer.
///
/// This struct intentionally mirrors the Python reference implementation (`lexer.py`).
/// In particular:
/// - `token_type` is a string like "ID", "INTEGER", "PLUS", ...
/// - `value` is the lexeme
/// - `lineno` is 0-based
#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: String,
    pub value: String,
    pub lineno: usize,
}

impl Token {
    pub fn new(token_type: impl Into<String>, value: impl Into<String>, lineno: usize) -> Self {
        Self {
            token_type: token_type.into(),
            value: value.into(),
            lineno,
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Similar to Python repr: Token(type=..., value=..., lineno=...)
        write!(
            f,
            "Token(type={:?}, value={:?}, lineno={:?})",
            self.token_type, self.value, self.lineno
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    IllegalCharacter { ch: char, lineno: usize },
    UnexpectedEof,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::IllegalCharacter { ch, lineno } => {
                write!(f, "Lexical error: illegal character '{ch}' at line {lineno}")
            }
            LexError::UnexpectedEof => write!(f, "Lexical error: unexpected EOF"),
        }
    }
}

impl Error for LexError {}

/// The Wend lexer (Rust).
///
/// Goal: behavior-compatible with the Python reference lexer:
/// - supports // line comments
/// - supports identifiers, integers, strings (with escaped quotes like \" )
/// - recognizes keywords, single-char and double-char operators
/// - maintains 0-based line numbers
pub struct WendLexer;

impl WendLexer {
    pub fn new() -> Self {
        WendLexer
    }

    /// Tokenize a whole source string.
    pub fn tokenize(&self, text: &str) -> Result<Vec<Token>, LexError> {
        // We operate on bytes for efficient lookahead; Wend is ASCII-oriented.
        let bytes = text.as_bytes();

        let mut tokens: Vec<Token> = Vec::new();

        let mut lineno: usize = 0;
        let mut idx: usize = 0;

        let mut token_start_lineno: usize = 0;

        // 0 = start, 1 = comment, 2 = string, 3 = word, 4 = number
        let mut state: u8 = 0;
        let mut accum = String::new();



        while idx < bytes.len() {
            let c = bytes[idx] as char;

            match state {
                // =========================
                // state 0: start
                // =========================
                0 => {
                    if c == '/' && idx + 1 < bytes.len() && bytes[idx + 1] == b'/' {
                        state = 1; // comment
                        idx += 1;  // consume second '/'
                    } else if c.is_ascii_alphabetic() || c == '_' {
                        state = 3;
                        token_start_lineno = lineno;
                        accum.clear();
                        accum.push(c);
                    } else if c.is_ascii_digit() {
                        state = 4;
                        token_start_lineno = lineno;
                        accum.clear();
                        accum.push(c);
                    } else if c == '"' {
                        state = 2;
                        token_start_lineno = lineno;
                        accum.clear();
                    } else if idx + 1 < bytes.len() {
                        let two = String::from_utf8_lossy(&bytes[idx..idx + 2]).to_string();
                        if let Some(tt) = double_char_token_type(&two) {
                            tokens.push(Token::new(tt, two, lineno));
                            idx += 1; // consume second char
                        } else if let Some(tt) = single_char_token_type(c) {
                            tokens.push(Token::new(tt, c.to_string(), lineno));
                        } else if c == '\n' {
                            lineno += 1;
                        } else if !matches!(c, ' ' | '\t' | '\r') {
                            return Err(LexError::IllegalCharacter { ch: c, lineno });
                        }
                    } else if let Some(tt) = single_char_token_type(c) {
                        tokens.push(Token::new(tt, c.to_string(), lineno));
                    } else if c == '\n' {
                        lineno += 1;
                    } else if !matches!(c, ' ' | '\t' | '\r') {
                        return Err(LexError::IllegalCharacter { ch: c, lineno });
                    }
                }

                // =========================
                // state 1: comment
                // =========================
                1 => {
                    if c == '\n' {
                        lineno += 1;
                        state = 0;
                    }
                }

                // =========================
                // state 2: string
                // =========================
                2 => {
                    if c == '"' && !accum.ends_with('\\') {
                        tokens.push(Token::new("STRING", accum.clone(), token_start_lineno));
                        accum.clear();
                        state = 0;
                    } else {
                        accum.push(c);
                    }
                }

                // =========================
                // state 3: word
                // =========================
                3 => {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        accum.push(c);
                    } else {
                        let tt = keyword_token_type(&accum).unwrap_or("ID");
                        tokens.push(Token::new(tt, accum.clone(), token_start_lineno));
                        accum.clear();
                        state = 0;
                        continue; // ⚠️ 关键：重新处理当前字符（不回退）
                    }
                }

                // =========================
                // state 4: number
                // =========================
                4 => {
                    if c.is_ascii_digit() {
                        accum.push(c);
                    } else {
                        tokens.push(Token::new("INTEGER", accum.clone(), token_start_lineno));
                        accum.clear();
                        state = 0;
                        continue; // ⚠️ 同上
                    }
                }

                _ => unreachable!(),
            }

            idx += 1;
        }

        // Python behavior: if state != 0 at EOF => unexpected EOF.
        // if state != 0 {
        //     return Err(LexError::UnexpectedEof);
        // }
        match state {
            0 | 1 => {}
            2 => return Err(LexError::UnexpectedEof),
            3 => {
                let tt = keyword_token_type(&accum).unwrap_or("ID");
                tokens.push(Token::new(tt, accum, token_start_lineno));
            }
            4 => {
                tokens.push(Token::new("INTEGER", accum, token_start_lineno));
            }
            _ => unreachable!(),
        }

        Ok(tokens)
    }
}

/// Keyword table (Python: keywords dict).
/// Returns the token type string if `word` is a reserved keyword.
fn keyword_token_type(word: &str) -> Option<&'static str> {
    match word {
        "true" | "false" => Some("BOOLEAN"),
        "print" | "println" => Some("PRINT"),
        "int" | "bool" => Some("TYPE"),
        "if" => Some("IF"),
        "else" => Some("ELSE"),
        "while" => Some("WHILE"),
        "return" => Some("RETURN"),
        _ => None,
    }
}

/// Two-character operator table (Python: double_char dict).
fn double_char_token_type(two: &str) -> Option<&'static str> {
    match two {
        "==" | "<=" | ">=" | "!=" => Some("COMP"),
        "&&" => Some("AND"),
        "||" => Some("OR"),
        _ => None,
    }
}

/// One-character operator table (Python: single_char dict).
fn single_char_token_type(c: char) -> Option<&'static str> {
    match c {
        '=' => Some("ASSIGN"),
        '<' | '>' => Some("COMP"),
        '!' => Some("NOT"),
        '+' => Some("PLUS"),
        '-' => Some("MINUS"),
        '/' => Some("DIVIDE"),
        '*' => Some("TIMES"),
        '%' => Some("MOD"),
        '(' => Some("LPAREN"),
        ')' => Some("RPAREN"),
        '{' => Some("BEGIN"),
        '}' => Some("END"),
        ';' => Some("SEMICOLON"),
        ',' => Some("COMMA"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_matches_python_behavior_basics() {
        let input = "0 // comment\n\"a\\\"b\"\n1337\n_identifier\nwhile\ntruefalse\n5*3 = -6\n2 && 2 == 5;\n";
        let tokens = WendLexer::new().tokenize(input).expect("tokenize failed");

        let expected = vec![
            Token::new("INTEGER", "0", 0),
            Token::new("STRING", "a\\\"b", 1),
            Token::new("INTEGER", "1337", 2),
            Token::new("ID", "_identifier", 3),
            Token::new("WHILE", "while", 4),
            Token::new("ID", "truefalse", 5),
            Token::new("INTEGER", "5", 6),
            Token::new("TIMES", "*", 6),
            Token::new("INTEGER", "3", 6),
            Token::new("ASSIGN", "=", 6),
            Token::new("MINUS", "-", 6),
            Token::new("INTEGER", "6", 6),
            Token::new("INTEGER", "2", 7),
            Token::new("AND", "&&", 7),
            Token::new("INTEGER", "2", 7),
            Token::new("COMP", "==", 7),
            Token::new("INTEGER", "5", 7),
            Token::new("SEMICOLON", ";", 7),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_keywords_and_boolean() {
        let input = "print println int bool if else while return true false";
        let tokens = WendLexer::new().tokenize(input).unwrap();
        let types: Vec<&str> = tokens.iter().map(|t| t.token_type.as_str()).collect();
        assert_eq!(
            types,
            vec![
                "PRINT", "PRINT", "TYPE", "TYPE", "IF", "ELSE", "WHILE", "RETURN", "BOOLEAN",
                "BOOLEAN"
            ]
        );
    }

    #[test]
    fn test_unexpected_eof_in_string() {
        let input = "\"unterminated";
        let err = WendLexer::new().tokenize(input).unwrap_err();
        assert_eq!(err, LexError::UnexpectedEof);
    }
}
