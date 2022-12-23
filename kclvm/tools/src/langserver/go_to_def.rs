



use kclvm_error::Position;
use kclvm_parser::load_program;
use kclvm_sema::resolver::{resolve_program, scope::{ScopeObject, Scope}};

use super::word_at_pos;

/// Get the definition of an identifier.
pub fn go_to_def(pos: Position) -> Option<Position> {
    Some(pos)
}

pub fn inner_most_test(pos: Position) -> Option<Scope>{
    let mut program = load_program(&[&pos.filename], None).unwrap();
    let scope = resolve_program(&mut program);
    let name = word_at_pos(pos.clone());
    match name{
        Some(name) => {
            println!("{:?}", "1");
            for s in scope.scope_map.values(){
                println!("{:?}", s.borrow_mut().start);
                println!("{:?}", s.borrow_mut().end);
                match s.borrow().inner_most(&pos){
                    Some(s) => return Some(s),
                    None => {},
                }
            }
            println!("{:?}", "2");
            return None
        }
        None => None
    }
}

/// Get the definition of an identifier.
pub fn go_to_def_test(path: &str, pos: Position) -> Option<Position> {
    let mut program = load_program(&[path], None).unwrap();
    let scope = resolve_program(&mut program);
    // let a = scope.main_scope().unwrap().borrow_mut();
    let name = word_at_pos(pos.clone());
    println!("{:?}", name);
    



    // for s in scope.scope_map.values() {
    //     let s = s.borrow_mut();
    //     // println!("scopekind: {:?}", s.kind);
    //     for (name, obj) in &s.elems{
    //         println!("{:?}", name);

    //     }
    // }
    if name.is_some(){
        let name = name.unwrap();

        let mut scopes = vec![];

        for s in scope.scope_map.values(){
            scopes.push(s.clone());
        }
    
        while !scopes.is_empty(){
            let s = scopes.pop().unwrap().clone();
            let s = s.borrow_mut();
            // let s = scopes.pop().unwrap().borrow_mut();
            match &s.lookup(&name) {
                Some(obj) => println!("find {:?}", obj),
                None => {
                    for c in &s.children{
                        scopes.push(c.clone());
                    }
                },
            }
        }
    }

    println!("{:?}", "");

    for s in scope.scope_map.values() {
        let s = s.borrow_mut().clone();
        println!("{:?}", s.start);
        println!("{:?}", s.end);
        for (name, obj) in s.elems {
            println!("obj {:?}", name);
            println!("{:?}", obj.borrow().start);
            println!("{:?}", obj.borrow().end);
        }
        for c in s.children {
            let c = c.borrow_mut();
            println!("obj kind{:?}", c.kind);
            println!("{:?}", c.start);
            println!("{:?}", c.end);
            for (name, obj) in &c.elems {
                println!("child elem: {:?}", name);
            }
            println!("{:?}", c.children);
        }
    }


    Some(pos)
}