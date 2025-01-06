use std::rc::Rc;
use std::cell::RefCell;
use crate::{car, cdr, evaluate, value_float, value_int, value_text, value_type};
use crate::nodes::Node;
use crate::stackframe::StackFrame;
use crate::values::{Lambda, Value, ValueType};

#[derive(Debug)]
#[derive(Clone)]
pub struct BuiltinLispFunction {
    pub name: &'static str,
    pub func: fn(Rc<RefCell<StackFrame>>, Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String>,
    pub lazy: bool,
}



inventory::collect!(BuiltinLispFunction);

fn evaluate_car(_stackframe: Rc<RefCell<StackFrame>>, node: Rc<RefCell<Node>>) -> Result<Rc<RefCell<Node>>, String> {
    let arg1 = crate::car(&node)?;
    if !arg1.borrow().is_list() {
        return Err("invalid argument to car".to_string());
    }
    let car = crate::car(&arg1)?;
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
