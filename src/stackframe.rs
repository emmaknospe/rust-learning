use std::rc::Rc;
use std::cell::RefCell;
use crate::nodes::Node;

pub struct VariableDefinition {
    pub name: String,
    pub value: Rc<RefCell<Node>>,
}

pub struct StackFrame {
    pub parent: Option<Rc<RefCell<StackFrame>>>,
    pub names: Vec<VariableDefinition>,
}

impl StackFrame {
    pub fn lookup(&self, name: &str) -> Option<Rc<RefCell<Node>>> {
        for item in self.names.iter() {
            if item.name == name {
                return Some(item.value.clone());
            }
        }
        if self.parent.is_some() {
            return self.parent.as_ref().unwrap().borrow().lookup(name);
        }
        None
    }
}
