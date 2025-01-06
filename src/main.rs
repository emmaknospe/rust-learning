use std::cell::{RefCell};
use std::cmp::PartialEq;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::rc::Rc;
use inventory;

const DEBUG: bool = false;
const REPL_MODE: bool = true;

fn main() {
    if REPL_MODE {
        repl();
    } else {
        execute_file("test.scm");
    }
}


fn execute_file(filename: &str) {
    let file = File::open(filename).unwrap();
    let mut reader = BufReader::new(file);
    execute(&mut reader);
}

fn repl() {
    loop {
        print!("lisp>");
        // flush the print buffer
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let mut reader = input.as_bytes();
        execute(&mut reader);
        println!();
    }
}


fn build_root_stackframe() -> Rc<RefCell<StackFrame>> {
    let stackframe = Rc::new(RefCell::new(StackFrame {
        parent: None,
        names: Vec::new(),
    }));
    // add to names
    stackframe.borrow_mut().names.push(VariableDefinition {
        name: "nil".to_string(),
        value: Rc::new(RefCell::new(Node::new())),
    });
    for item in inventory::iter::<BuiltinLispFunction> {
        stackframe.borrow_mut().names.push(VariableDefinition {
            name: item.name.to_string(),
            value: Rc::new(RefCell::new(Node {
                car: None,
                cdr: None,
                value: Some(
                    Value {
                        value_type: ValueType::BuiltIn,
                        string_value: Some(item.name.to_string()),
                        int_value: None,
                        float_value: None,
                        lambda_value: None,
                        builtin_value: Some(item.clone()),
                    }
                ),
            })),
        });
    }
    stackframe
}

fn execute(data: &mut impl BufRead) {
    // create vector of tokens
    let mut tokens = Vec::new();
    tokenize(data, &mut tokens).expect("failed to tokenize");

    // parse tokens
    let root_node = parse_2(tokens).expect("failed to parse");
    if !root_node.borrow().is_valid() {
        println!("Error: invalid parse tree");
        std::process::exit(1);
    }
    let root_stackframe = build_root_stackframe();

    display_tree(evaluate_each(root_stackframe, root_node).expect("failed to evaluate"));
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
enum TokenType {
    Symbol,
    String,
    Integer,
    Float,
    OpenParen,
    CloseParen,
}

impl Default for ValueType {
    fn default() -> Self {
        ValueType::Symbol
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
struct Token {
    text: Option<String>,
    token_type: TokenType,
    line_num: u32,
    col_num: u32,
}


#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
enum ValueType {
    Symbol,
    String,
    Integer,
    Float,
    Lambda,
    BuiltIn,
}

#[derive(Debug)]
#[derive(Clone)]
struct Value {
    value_type: ValueType,
    string_value: Option<String>,
    int_value: Option<i32>,
    float_value: Option<f32>,
    lambda_value: Option<Lambda>,
    builtin_value: Option<BuiltinLispFunction>,
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
    fn from_string(string: String) -> Value {
        Value {
            value_type: ValueType::String,
            string_value: Some(string),
            int_value: None,
            float_value: None,
            lambda_value: None,
            builtin_value: None,
        }
    }
    fn from_int(int: i32) -> Value {
        Value {
            value_type: ValueType::Integer,
            string_value: None,
            int_value: Some(int),
            float_value: None,
            lambda_value: None,
            builtin_value: None,
        }
    }
    fn from_float(float: f32) -> Value {
        Value {
            value_type: ValueType::Float,
            string_value: None,
            int_value: None,
            float_value: Some(float),
            lambda_value: None,
            builtin_value: None,
        }
    }
    fn from_symbol(symbol: String) -> Value {
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

fn make_value_from_token(token: Token) -> Result<Value, String> {
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
struct Lambda {
    params: Vec<String>,
    body: Rc<RefCell<Node>>,
}

fn tokenize(data: &mut impl BufRead, final_tokens: &mut Vec<Token>) -> Result<String, String> {
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

#[derive(Debug)]
pub struct Node {
    car: Option<Rc<RefCell<Node>>>,
    cdr: Option<Rc<RefCell<Node>>>,
    value: Option<Value>,
}


impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.car == other.car && self.cdr == other.cdr && self.value == other.value
    }
}

impl Node {
    fn new() -> Node {
        Node {
            car: None,
            cdr: None,
            value: None,
        }
    }
    fn car(&self) -> Option<Rc<RefCell<Node>>> {
        self.car.clone()
    }
    fn cdr(&self) -> Option<Rc<RefCell<Node>>> {
        self.cdr.clone()
    }
    fn is_value(&self) -> bool {
        self.value.is_some()
    }
    fn is_list(&self) -> bool {
        self.car.is_some() && self.cdr.is_some()
    }
    fn is_null(&self) -> bool {
        !self.car.is_some() && !self.value.is_some() && !self.cdr.is_some()
    }
    fn is_valid(&self) -> bool {
        if self.is_value() {
            return true;
        }
        if self.is_list() {
            return self.car.as_ref().unwrap().borrow().is_valid() && self.cdr.as_ref().unwrap().borrow().is_valid();
        }
        if self.is_null() {
            return true;
        }
        println!("Error: invalid node");
        print!("invalid node: {:?}", self);
        println!();
        false
    }
    fn value(&self) -> Option<Value> {
        self.value.clone()
    }
}


fn parse_2(tokens: Vec<Token>) -> Result<Rc<RefCell<Node>>, String> {
    let mut tokens = tokens.clone();
    let mut cur_node = Rc::new(RefCell::new(Node::new()));
    let mut stack = Vec::new();
    loop {
        let cur_token = tokens.pop();
        if cur_token.is_none() {
            break;
        }
        let cur_token = cur_token.unwrap();
        if cur_token.token_type == TokenType::CloseParen {
            stack.push(cur_node.clone());
            cur_node = Rc::new(RefCell::new(Node::new()));
        } else if cur_token.token_type == TokenType::OpenParen {
            let node_up_stack = stack.pop().unwrap();
            cur_node = Rc::new(RefCell::new(Node {
                car: Some(cur_node.clone()),
                cdr: Some(node_up_stack.clone()),
                value: None,
            }));
        } else {
            let value_node = Rc::new(RefCell::new(Node {
                car: None,
                cdr: None,
                value: Some(make_value_from_token(cur_token)?),
            }));
            cur_node = Rc::new(RefCell::new(Node {
                car: Some(value_node.clone()),
                cdr: Some(cur_node.clone()),
                value: None,
            }));
        }
    }
    Ok(cur_node)
}



fn display_tree(node: Rc<RefCell<Node>>) {
    if node.borrow().is_null() {
        return;
    }
    if node.borrow().is_value() {
        let value = node.borrow().value().unwrap();
        match value.value_type {
            ValueType::Symbol => { print!("{} ", value.string_value.as_ref().unwrap()); },
            ValueType::String => { print!("\"{}\" ", value.string_value.as_ref().unwrap()); },
            ValueType::Integer => { print!("{} ", value.int_value.as_ref().unwrap()); },
            ValueType::Float => { print!("{} ", value.float_value.as_ref().unwrap()); },
            ValueType::Lambda => { print!("lambda({:?})", value.lambda_value.clone().unwrap().params); },
            _ => { println!("\n Error: invalid token {:?}", value); std::process::exit(1); },
        }
    } else {
        node.borrow().car().and_then(
            |car| {
                if car.borrow().is_list() {
                    print!("(");
                    display_tree(car.clone());
                    print!(")");
                    Some(car)
                } else {
                    display_tree(car.clone());
                    Some(car)
                }
            }
        );
        node.borrow().cdr().and_then(
            |cdr| {
                if cdr.borrow().is_list() {
                    display_tree(cdr.clone());
                    Some(cdr)
                } else {
                    print!(" . ");
                    display_tree(cdr.clone());
                    Some(cdr)
                }
            }
        );
    }
}

fn evaluate_each(stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let mut new_node = Rc::new(RefCell::new(Node::new()));
    if node.borrow().is_null() {
        return Ok(new_node);
    }
    if node.borrow().is_value() {
        return Err("invalid list of nodes".to_string());
    }
    if node.borrow().is_list() {
        let car = node.borrow().car.as_ref().unwrap().clone();
        let cdr = node.borrow().cdr.as_ref().unwrap().clone();
        let new_car = evaluate(stackframe.clone(), car)?;
        let new_cdr = evaluate_each(stackframe, cdr)?;
        new_node = Rc::new(RefCell::new(Node {
            car: Some(new_car.clone()),
            cdr: Some(new_cdr.clone()),
            value: None,
        }));
        return Ok(new_node);
    }
    Err("invalid node".to_string())
}

fn value_type(node: Rc<RefCell<Node>>) -> ValueType {
    let borrowed = node.borrow();
    let value = borrowed.value.as_ref().unwrap();
    value.value_type.clone()
}

fn value_text(node: Rc<RefCell<Node>>) -> String {
    let borrowed = node.borrow();
    let value = borrowed.value.as_ref().unwrap();
    value.string_value.as_ref().unwrap().clone()
}

fn value_int(node: Rc<RefCell<Node>>) -> i32 {
    let borrowed = node.borrow();
    let value = borrowed.value.as_ref().unwrap();
    value.int_value.as_ref().unwrap().clone()
}

fn value_float(node: Rc<RefCell<Node>>) -> f32 {
    let borrowed = node.borrow();
    let value = borrowed.value.as_ref().unwrap();
    value.float_value.as_ref().unwrap().clone()
}


#[derive(Debug)]
#[derive(Clone)]
pub struct BuiltinLispFunction {
    pub name: &'static str,
    pub func: fn(Rc<RefCell<StackFrame>>, Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String>,
    pub lazy: bool,
}

inventory::collect!(BuiltinLispFunction);


struct VariableDefinition {
    name: String,
    value: Rc<RefCell<Node>>,
}

pub struct StackFrame {
    parent: Option<Rc<RefCell<StackFrame>>>,
    names: Vec<VariableDefinition>,
}


fn lookup(stackframe: Rc<RefCell<StackFrame>>, name: &str) -> Option<Rc<RefCell<Node>>> {
    let borrowed = stackframe.borrow();
    for item in borrowed.names.iter() {
        if item.name == name {
            return Some(item.value.clone());
        }
    }
    if borrowed.parent.is_some() {
        return lookup(borrowed.parent.as_ref().unwrap().clone(), name);
    }
    None
}

fn evaluate(stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    if DEBUG {
        println!("#######################");
        println!("evaluate on node");
        display_tree(node.clone());
        println!();
        println!("#######################");
    }
    if node.borrow().is_value() {
        return if value_type(node.clone()) == ValueType::Symbol {
            let text = value_text(node.clone());
            lookup(stackframe, &text).ok_or(format!("undefined symbol '{}'", text))
        } else if value_type(node.clone()) == ValueType::Integer {
            Ok(node)
        } else if value_type(node.clone()) == ValueType::Float {
            Ok(node)
        } else if value_type(node.clone()) == ValueType::String {
            Ok(node)
        } else {
            Err("invalid value".to_string())
        }
    }
    let operation = evaluate(stackframe.clone(), car(&node)?)?;
    let args = cdr(&node)?;
    if !operation.borrow().is_value() {
        println!("Error: invalid operation");
        display_tree(operation.clone());
        return Err("invalid operation: not a value".to_string());
    }
    let borrowed = operation.borrow();
    let operation_value = borrowed.value.as_ref().unwrap();
    if operation_value.value_type == ValueType::Lambda {
        apply_lambda(stackframe.clone(), operation_value.lambda_value.as_ref().unwrap(), args)
    } else if operation_value.value_type == ValueType::BuiltIn {
        let builtin = operation_value.builtin_value.as_ref().unwrap();
        // evaluate args if not lazy
        if !builtin.lazy {
            return (builtin.func)(stackframe.clone(), evaluate_each(stackframe, args)?);
        } else {
            return (builtin.func)(stackframe, args);
        }
    } else {
        return Err("invalid operation: not a function".to_string());
    }
}


fn apply_lambda(stackframe: Rc<RefCell<StackFrame>>, lambda: &Lambda, args: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let new_stackframe = Rc::new(RefCell::new(StackFrame {
        parent: Some(stackframe.clone()),
        names: Vec::new(),
    }));
    let mut args_node = args.clone();
    for param in lambda.params.iter() {
        if args_node.borrow().is_null() {
            return Err("not enough arguments".to_string());
        }
        let arg = evaluate(stackframe.clone(), car(&args_node)?)?;
        new_stackframe.borrow_mut().names.push(VariableDefinition {
            name: param.clone(),
            value: arg.clone(),
        });
        args_node = cdr(&args_node)?;
    }
    if !args_node.borrow().is_null() {
        return Err("too many arguments".to_string());
    }
    evaluate(new_stackframe, lambda.body.clone())
}

fn evaluate_car(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let arg1 = car(&node)?;
    if !arg1.borrow().is_list() {
        return Err("invalid argument to car".to_string());
    }
    let car = car(&arg1)?;
    Ok(car)
}

inventory::submit! {
    BuiltinLispFunction {
        name: "car",
        func: evaluate_car,
        lazy: false,
    }
}

fn evaluate_cdr(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let arg1 = car(&node)?;
    if !arg1.borrow().is_list() {
        return Err("invalid argument to cdr".to_string());
    }
    let cdr = cdr(&arg1)?;
    Ok(cdr)
}


inventory::submit! {
    BuiltinLispFunction {
        name: "cdr",
        func: evaluate_cdr,
        lazy: false,
    }
}

fn evaluate_cons(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let arg1 = car(&node)?;
    let arg2 = car(&cdr(&node)?)?;
    Ok(Rc::new(RefCell::new(Node {
        car: Some(arg1.clone()),
        cdr: Some(arg2.clone()),
        value: None,
    })))
}


inventory::submit! {
    BuiltinLispFunction {
        name: "cons",
        func: evaluate_cons,
        lazy: false,
    }
}


fn evaluate_add(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let mut accumulator_int = 0;
    let mut accumulator_float = 0.0;
    let mut is_float = false;
    let mut node = node.clone();
    while !node.borrow().is_null() {
        let arg = car(&node)?;
        if value_type(arg.clone()) == ValueType::Integer {
            accumulator_int += value_int(arg.clone());
        } else if value_type(arg.clone()) == ValueType::Float {
            if !is_float {
                accumulator_float = accumulator_int as f32;
            }
            accumulator_float += value_float(arg.clone());
            is_float = true;
        } else {
            return Err("invalid argument to +".to_string());
        }
        node = cdr(&node)?;
    }
    if is_float {
        build_float_node(accumulator_float)
    } else {
        build_int_node(accumulator_int)
    }
}


inventory::submit! {
    BuiltinLispFunction {
        name: "+",
        func: evaluate_add,
        lazy: false,
    }
}


fn evaluate_multiply(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let mut accumulator_int = 1;
    let mut accumulator_float = 1.0;
    let mut is_float = false;
    let mut node = node.clone();
    while !node.borrow().is_null() {
        let arg = car(&node)?;
        if value_type(arg.clone()) == ValueType::Integer {
            accumulator_int *= value_int(arg.clone());
        } else if value_type(arg.clone()) == ValueType::Float {
            if !is_float {
                accumulator_float = accumulator_int as f32;
            }
            accumulator_float *= value_float(arg.clone());
            is_float = true;
        } else {
            return Err("invalid argument to *".to_string());
        }
        node = cdr(&node)?;
    }
    if is_float {
        build_float_node(accumulator_float)
    } else {
        build_int_node(accumulator_int)
    }
}


inventory::submit! {
    BuiltinLispFunction {
        name: "*",
        func: evaluate_multiply,
        lazy: false,
    }
}

fn evaluate_lambda(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let params = car(&node)?;
    let mut params_vec = Vec::new();
    let mut params_node = params.clone();
    while !params_node.borrow().is_null() {
        let param = car(&params_node)?;
        if value_type(param.clone()) != ValueType::Symbol {
            return Err("invalid lambda parameter".to_string());
        }
        params_vec.push(value_text(param.clone()));
        params_node = cdr(&params_node)?;
    }
    let body = car(&cdr(&node)?)?;
    Ok(Rc::new(RefCell::new(Node {
        car: None,
        cdr: None,
        value: Some(
            Value {
                value_type: ValueType::Lambda,
                lambda_value: Some(Lambda {
                    params: params_vec,
                    body: body.clone(),
                }),
                builtin_value: None,
                string_value: None,
                int_value: None,
                float_value: None,
            }
        ),
    })))
}


inventory::submit! {
    BuiltinLispFunction {
        name: "lambda",
        func: evaluate_lambda,
        lazy: true,
    }
}

fn build_int_node(accumulator_int: i32) -> Result<Rc<RefCell<Node>>, String> {
    Ok(Rc::new(RefCell::new(Node {
        car: None,
        cdr: None,
        value: Some(
            Value::from_int(accumulator_int)
        ),
    })))
}

fn evaluate_subtract(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let mut accumulator_int = 0;
    let mut accumulator_float = 0.0;
    let mut is_float = false;
    let mut node = node.clone();
    let arg1 = car(&node)?;
    if value_type(arg1.clone()) == ValueType::Integer {
        accumulator_int = value_int(arg1.clone());
    } else if value_type(arg1.clone()) == ValueType::Float {
        accumulator_float = value_float(arg1.clone());
        is_float = true;
    } else {
        return Err("invalid argument to -".to_string());
    }
    node = cdr(&node)?;
    while !node.borrow().is_null() {
        let arg = car(&node)?;
        if value_type(arg.clone()) == ValueType::Integer {
            accumulator_int -= value_int(arg.clone());
        } else if value_type(arg.clone()) == ValueType::Float {
            if !is_float {
                accumulator_float = accumulator_int as f32;
            }
            accumulator_float -= value_float(arg.clone());
            is_float = true;
        } else {
            return Err("invalid argument to -".to_string());
        }
        node = cdr(&node)?;
    }
    if is_float {
        build_float_node(accumulator_float)
    } else {
        build_int_node(accumulator_int)
    }
}


inventory::submit! {
    BuiltinLispFunction {
        name: "-",
        func: evaluate_subtract,
        lazy: false,
    }
}


fn evaluate_divide(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    // division always returns a float
    let mut accumulator_float;
    let mut node = node.clone();
    let arg1 = car(&node)?;
    if value_type(arg1.clone()) == ValueType::Integer {
        accumulator_float = value_int(arg1.clone()) as f32;
    } else if value_type(arg1.clone()) == ValueType::Float {
        accumulator_float = value_float(arg1.clone());
    } else {
        return Err("invalid argument to /".to_string());
    }
    node = cdr(&node)?;
    while !node.borrow().is_null() {
        let arg = car(&node)?;
        if value_type(arg.clone()) == ValueType::Integer {
            accumulator_float /= value_int(arg.clone()) as f32;
        } else if value_type(arg.clone()) == ValueType::Float {
            accumulator_float /= value_float(arg.clone());
        } else {
            return Err("invalid argument to /".to_string());
        }
        node = cdr(&node)?;
    }
    if accumulator_float.is_infinite() {
        return Err("division by zero".to_string());
    }
    build_float_node(accumulator_float)
}


inventory::submit! {
    BuiltinLispFunction {
        name: "/",
        func: evaluate_divide,
        lazy: false,
    }
}

fn build_float_node(float: f32) -> Result<Rc<RefCell<Node>>, String> {
    Ok(Rc::new(RefCell::new(Node {
        car: None,
        cdr: None,
        value: Some(
            Value::from_float(float)
        ),
    })))
}

fn is_truthy(node: Rc<RefCell<Node>>) -> bool {
    if node.borrow().is_null() {
        return false;
    }
    if node.borrow().is_value() {
        let borrowed = node.borrow();
        let value = borrowed.value.as_ref().unwrap();
        match value.value_type {
            ValueType::Symbol => { return value_text(node.clone()) != "nil"; },
            ValueType::String => { return value_text(node.clone()) != ""; },
            ValueType::Integer => { return value_int(node.clone()) != 0; },
            ValueType::Float => { return value_float(node.clone()) != 0.0; },
            ValueType::Lambda => { return true; },
            _ => { println!("\n Error: invalid token {:?}", value); std::process::exit(1); },
        }
    }
    if node.borrow().is_list() {
        return true;
    }
    false
}


fn evaluate_if(stackframe: Rc<RefCell<StackFrame>>, args: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let condition = car(&args)?;
    let if_true = car(&cdr(&args)?)?;
    let if_false = car(&cdr(&cdr(&args)?)?)?;
    let condition_evaluated = evaluate(stackframe.clone(), condition)?;
    if is_truthy(condition_evaluated.clone()) {
        return Ok(evaluate(stackframe, if_true)?);
    }
    Ok(evaluate(stackframe, if_false)?)
}


inventory::submit! {
    BuiltinLispFunction {
        name: "if",
        func: evaluate_if,
        lazy: true,
    }
}

fn cdr(args: &Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    args.borrow().cdr.as_ref().map_or_else(
        || Err(format!("missing cdr: {:?}", args)),
        |cdr| Ok(cdr.clone())
    )
}

fn car(args: &Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    args.borrow().car.as_ref().map_or_else(
        || Err(format!("missing car: {:?}", args)),
        |car| Ok(car.clone())
    )
}
