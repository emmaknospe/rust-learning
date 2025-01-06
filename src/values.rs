use std::rc::Rc;
use std::cell::RefCell;
use crate::builtins::BuiltinLispFunction;
use crate::nodes::Node;
use crate::tokens::{Token, TokenType};

impl Default for ValueType {
    fn default() -> Self {
        ValueType::Symbol
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub enum ValueType {
    Symbol,
    String,
    Integer,
    Float,
    Lambda,
    BuiltIn,
}

#[derive(Debug)]
#[derive(Clone)]
pub struct Value {
    pub value_type: ValueType,
    pub string_value: Option<String>,
    pub int_value: Option<i32>,
    pub float_value: Option<f32>,
    pub lambda_value: Option<Lambda>,
    pub builtin_value: Option<BuiltinLispFunction>,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.value_type == other.value_type &&
            self.string_value == other.string_value &&
            self.int_value == other.int_value &&
            self.float_value == other.float_value &&
            self.value_type != ValueType::Lambda
    }
}

impl Value {
    pub fn from_string(string: String) -> Value {
        Value {
            value_type: ValueType::String,
            string_value: Some(string),
            int_value: None,
            float_value: None,
            lambda_value: None,
            builtin_value: None,
        }
    }
    pub fn from_int(int: i32) -> Value {
        Value {
            value_type: ValueType::Integer,
            string_value: None,
            int_value: Some(int),
            float_value: None,
            lambda_value: None,
            builtin_value: None,
        }
    }
    pub fn from_float(float: f32) -> Value {
        Value {
            value_type: ValueType::Float,
            string_value: None,
            int_value: None,
            float_value: Some(float),
            lambda_value: None,
            builtin_value: None,
        }
    }
    pub fn from_symbol(symbol: String) -> Value {
        Value {
            value_type: ValueType::Symbol,
            string_value: Some(symbol),
            int_value: None,
            float_value: None,
            lambda_value: None,
            builtin_value: None,
        }
    }
}

pub fn make_value_from_token(token: Token) -> Result<Value, String> {
    let value_type = match token.token_type {
        TokenType::Symbol => ValueType::Symbol,
        TokenType::String => ValueType::String,
        TokenType::Integer => ValueType::Integer,
        TokenType::Float => ValueType::Float,
        _ => {
            return Err("invalid token type".to_string());
        }
    };
    match value_type {
        ValueType::Symbol => {
            Ok(Value::from_symbol(token.text.unwrap()))
        }
        ValueType::String => {
            Ok(Value::from_string(token.text.unwrap()))
        }
        ValueType::Integer => {
            Ok(Value::from_int(token.text.unwrap().parse::<i32>().unwrap()))
        }
        ValueType::Float => {
            Ok(Value::from_float(token.text.unwrap().parse::<f32>().unwrap()))
        }
        _ => {
            Err("invalid value type".to_string())
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub body: Rc<RefCell<Node>>,
}