use std::io::{BufRead, BufReader};

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum TokenType {
    Symbol,
    String,
    Integer,
    Float,
    OpenParen,
    CloseParen,
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub struct Token {
    pub text: Option<String>,
    pub token_type: TokenType,
    pub line_num: u32,
    pub col_num: u32,
}

pub fn tokenize(data: &mut impl BufRead, final_tokens: &mut Vec<Token>) -> Result<String, String> {
    let mut n_open = 0;
    let mut line_num = 0;
    let mut reader = BufReader::new(data);
    let mut in_string = false;
    let mut cur_string = String::new();
    let mut cur_symbol = String::new();
    loop {
        let mut line = String::new();
        let n = reader.read_line(&mut line).unwrap();
        if n == 0 {
            break;
        }
        line_num += 1;
        let mut col_num = 0;
        for c in line.chars() {
            if in_string {
                if c == '"' {
                    in_string = false;
                    final_tokens.push(Token {
                        text: Some(cur_string.clone()),
                        token_type: TokenType::String,
                        line_num: line_num,
                        col_num: col_num,
                    });
                    cur_string.clear();
                } else {
                    cur_string.push(c);
                }
            } else {
                if c == '"' {
                    in_string = true;
                } else if c == '(' {
                    n_open += 1;
                    final_tokens.push(Token {
                        text: Some("(".to_string()),
                        token_type: TokenType::OpenParen,
                        line_num: line_num,
                        col_num: col_num,
                    });
                } else if c == ')' {
                    let res = close_symbol_int_float(final_tokens, line_num, &mut cur_symbol, col_num);
                    if res.is_err() {
                        return res;
                    }
                    n_open -= 1;
                    if n_open < 0 {
                        println!("Error: too many closing parens on line {}", line_num);
                        return Err("too many closing parens".to_string());
                    }
                    final_tokens.push(Token {
                        text: Some(")".to_string()),
                        token_type: TokenType::CloseParen,
                        line_num: line_num,
                        col_num: col_num,
                    });
                } else if c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == ',' {
                    if cur_symbol.len() > 0 {
                        let res = close_symbol_int_float(final_tokens, line_num, &mut cur_symbol, col_num);
                        if res.is_err() {
                            return res;
                        }
                    }
                } else {
                    cur_symbol.push(c);
                }
            }
            col_num += 1;
        }
    }
    if n_open > 0 {
        println!("Error: too many opening parens");
        return Err("too many opening parens".to_string());
    }
    Ok("".to_string())
}

fn close_symbol_int_float(final_tokens: &mut Vec<Token>, line_num: u32, cur_symbol: &mut String, col_num: u32) -> Result<String, String> {
    // close any open symbols, ints, or floats
    if cur_symbol.len() > 0 {
        if let Ok(_int_val) = cur_symbol.parse::<i32>() {
            final_tokens.push(Token {
                text: Some(cur_symbol.clone()),
                token_type: TokenType::Integer,
                line_num: line_num,
                col_num: col_num,
            });
        } else if let Ok(_float_val) = cur_symbol.parse::<f32>() {
            final_tokens.push(Token {
                text: Some(cur_symbol.clone()),
                token_type: TokenType::Float,
                line_num: line_num,
                col_num: col_num,
            });
        } else {
            // verify current symbol doesn't start with a number
            if cur_symbol.chars().next().unwrap().is_numeric() {
                let start_col_num = col_num - cur_symbol.len() as u32;
                println!("Error: invalid number on line {}, column {}", line_num, start_col_num);
                return Err("invalid number".to_string());
            }
            // check for special symbols
            if cur_symbol == "+" || cur_symbol == "-" || cur_symbol == "*" || cur_symbol == "/" {
                final_tokens.push(Token {
                    text: Some(cur_symbol.clone()),
                    token_type: TokenType::Symbol,
                    line_num: line_num,
                    col_num: col_num,
                });
                cur_symbol.clear();
                return Ok("".to_string());
            }
            // verify there are no invalid characters in the symbol
            let mut char_num = 0;
            for c in cur_symbol.chars() {
                if c.is_alphanumeric() || c == '_' {
                    char_num += 1;
                    continue;
                }
                let start_col_num = col_num - cur_symbol.len() as u32 + char_num;
                println!("Error: invalid character in symbol '{}' on line {}, column {}", c, line_num, start_col_num);
                return Err("invalid character".to_string());
            }
            final_tokens.push(Token {
                text: Some(cur_symbol.clone()),
                token_type: TokenType::Symbol,
                line_num: line_num,
                col_num: col_num,
            });
        }
        cur_symbol.clear();
    }
    Ok("".to_string())
}