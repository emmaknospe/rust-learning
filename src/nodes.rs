use std::rc::Rc;
use std::cell::RefCell;
use crate::values::Value;

#[derive(Debug)]
pub struct Node {
    pub car: Option<Rc<RefCell<Node>>>,
    pub cdr: Option<Rc<RefCell<Node>>>,
    pub value: Option<Value>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.car == other.car && self.cdr == other.cdr && self.value == other.value
    }
}

impl Node {
    pub fn new() -> Node {
        Node {
            car: None,
            cdr: None,
            value: None,
        }
    }
    pub fn car(&self) -> Option<Rc<RefCell<Node>>> {
        self.car.clone()
    }
    pub fn cdr(&self) -> Option<Rc<RefCell<Node>>> {
        self.cdr.clone()
    }
    pub fn is_value(&self) -> bool {
        self.value.is_some()
    }
    pub fn is_list(&self) -> bool {
        self.car.is_some() && self.cdr.is_some()
    }
    pub fn is_null(&self) -> bool {
        !self.car.is_some() && !self.value.is_some() && !self.cdr.is_some()
    }
    pub fn is_valid(&self) -> bool {
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
    pub fn value(&self) -> Option<Value> {
        self.value.clone()
    }
}