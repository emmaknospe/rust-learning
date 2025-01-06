use std::cell::RefCell;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::rc::Rc;
use inventory;
use builtins::BuiltinLispFunction;
use nodes::Node;
use stackframe::{StackFrame, VariableDefinition};
use values::{Lambda, Value, ValueType};

mod tokens;
mod values;
mod parse;
mod display;
mod nodes;
mod stackframe;
mod builtins;

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
    tokens::tokenize(data, &mut tokens).expect("failed to tokenize");

    // parse tokens
    let root_node = parse::parse_2(tokens).expect("failed to parse");
    if !root_node.borrow().is_valid() {
        println!("Error: invalid parse tree");
        std::process::exit(1);
    }
    let root_stackframe = build_root_stackframe();

    display::display_tree(evaluate_each(root_stackframe, root_node).expect("failed to evaluate"));
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


fn evaluate(stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    if DEBUG {
        println!("#######################");
        println!("evaluate on node");
        display::display_tree(node.clone());
        println!();
        println!("#######################");
    }
    if node.borrow().is_value() {
        return if value_type(node.clone()) == ValueType::Symbol {
            let text = value_text(node.clone());
            stackframe.borrow().lookup(&text).ok_or(format!("undefined symbol '{}'", text))
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
        display::display_tree(operation.clone());
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
        println!("Error: invalid operation");
        display::display_tree(operation.clone());
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
