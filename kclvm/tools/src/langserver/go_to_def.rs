use std::borrow::Borrow;

use kclvm_error::Position;
use kclvm_parser::load_program;
use kclvm_sema::resolver::resolve_program;

use super::word_at_pos;

/// Get the definition of an identifier.
pub fn go_to_def(pos: Position) -> Option<Position> {
    Some(pos)
}

/// Get the definition of an identifier.
pub fn go_to_def_test(path: &str, pos: Position) -> Option<Position> {
    let mut program = load_program(&[path], None).unwrap();
    let scope = resolve_program(&mut program);
    // let a = scope.main_scope().unwrap().borrow_mut();
    let name = word_at_pos(pos.clone());
    println!("{:?}", name);
    if name.is_some(){
        let name = name.unwrap();
        for s in scope.scope_map.values() {
            let s = s.borrow_mut();
            match s.lookup(&name) {
                Some(obj) => println!("{:?}", obj),
                None => {},
            }
        }
    }


    Some(pos)
}