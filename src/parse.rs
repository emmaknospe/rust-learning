use std::rc::Rc;
use std::cell::RefCell;
use crate::values;
use crate::nodes::Node;
use crate::tokens::{Token, TokenType};

pub fn parse_2(tokens: Vec<Token>) -> Result<Rc<RefCell<Node>>, String> {
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
                value: Some(values::make_value_from_token(cur_token)?),
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