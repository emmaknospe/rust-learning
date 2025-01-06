use std::rc::Rc;
use std::cell::RefCell;
use crate::nodes::Node;
use crate::values::ValueType;

pub fn display_tree(node: Rc<RefCell<Node>>) {
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
                } else if !cdr.borrow().is_null() {
                    print!(" . ");
                    display_tree(cdr.clone());
                    Some(cdr)
                } else {
                    Some(cdr)
                }
            }
        );
    }
}