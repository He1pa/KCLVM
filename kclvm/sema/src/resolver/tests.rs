use super::Options;
use super::Resolver;
use crate::builtin::BUILTIN_FUNCTION_NAMES;
use crate::pre_process::pre_process_program;
use crate::resolver::lint;
use crate::resolver::resolve_program;
use crate::resolver::scope::*;
use crate::ty::Type;
use indexmap::IndexSet;
use kclvm_ast::ast;
use kclvm_error::*;
use kclvm_parser::{load_program, parse_program};
use std::rc::Rc;

#[test]
fn test_scope() {
    let mut scope = builtin_scope();
    for name in BUILTIN_FUNCTION_NAMES {
        let obj = scope.lookup(name).unwrap();
        let obj_ref = obj.borrow_mut();
        assert!(obj_ref.ty.is_func());
    }
    for name in BUILTIN_FUNCTION_NAMES {
        scope.set_ty(name, Rc::new(Type::ANY));
    }
    for name in BUILTIN_FUNCTION_NAMES {
        let obj = scope.lookup(name).unwrap();
        let obj_ref = obj.borrow_mut();
        assert!(obj_ref.ty.is_any());
    }
}

#[test]
fn test_resolve_program() {
    let mut program = parse_program("./src/resolver/test_data/assign.k").unwrap();
    let scope = resolve_program(&mut program);
    assert_eq!(scope.pkgpaths(), vec!["__main__".to_string()]);
    let main_scope = scope.main_scope().unwrap();
    let main_scope = main_scope.borrow_mut();
    assert!(main_scope.lookup("a").is_some());
    assert!(main_scope.lookup("b").is_some());
    assert!(main_scope.lookup("print").is_none());
}

#[test]
fn test_pkg_init_in_schema_resolve() {
    let mut program =
        load_program(&["./src/resolver/test_data/pkg_init_in_schema.k"], None).unwrap();
    let scope = resolve_program(&mut program);
    assert_eq!(
        scope.pkgpaths(),
        vec!["__main__".to_string(), "pkg".to_string()]
    );
    let module = &program.pkgs["pkg"][0];
    if let ast::Stmt::Schema(schema) = &module.body[1].node {
        if let ast::Stmt::SchemaAttr(attr) = &schema.body[0].node {
            let value = attr.value.as_ref().unwrap();
            if let ast::Expr::Schema(schema_expr) = &value.node {
                assert_eq!(schema_expr.name.node.names, vec!["Name".to_string()]);
            } else {
                panic!("test failed, expect schema expr, got {:?}", value)
            }
        } else {
            panic!(
                "test failed, expect schema attribute, got {:?}",
                schema.body[0]
            )
        }
    } else {
        panic!(
            "test failed, expect schema statement, got {:?}",
            module.body[1]
        )
    }
}

#[test]
fn test_resolve_program_fail() {
    let mut program = parse_program("./src/resolver/test_fail_data/config_expr.k").unwrap();
    let scope = resolve_program(&mut program);
    assert_eq!(scope.diagnostics.len(), 1);
    let diag = &scope.diagnostics[0];
    assert_eq!(diag.code, Some(DiagnosticId::Error(ErrorKind::TypeError)));
    assert_eq!(diag.messages.len(), 1);
    assert_eq!(diag.messages[0].message, "expect int, got {str:int(1)}");
}

#[test]
fn test_resolve_program_cycle_reference_fail() {
    let mut program = load_program(
        &["./src/resolver/test_fail_data/cycle_reference/file1.k"],
        None,
    )
    .unwrap();
    let scope = resolve_program(&mut program);
    let err_messages = [
        "There is a circular import reference between module file1 and file2",
        "There is a circular reference between schema SchemaBase and SchemaSub",
        "There is a circular reference between schema SchemaSub and SchemaBase",
        "There is a circular reference between rule RuleBase and RuleSub",
        "There is a circular reference between rule RuleSub and RuleBase",
    ];
    assert_eq!(scope.diagnostics.len(), err_messages.len());
    for (diag, msg) in scope.diagnostics.iter().zip(err_messages.iter()) {
        assert_eq!(diag.messages[0].message, msg.to_string(),);
    }
}

#[test]
fn test_lint() {
    let mut program = load_program(&["./src/resolver/test_data/import.k"], None).unwrap();
    pre_process_program(&mut program);
    let mut resolver = Resolver::new(
        &program,
        Options {
            raise_err: true,
            config_auto_fix: false,
            lint_check: true,
        },
    );
    resolver.resolve_import();
    resolver.check(kclvm_ast::MAIN_PKG);

    let root = &program.root.clone();
    let filename = root.clone() + "/import.k";

    let mut diagnostics: IndexSet<Diagnostic> = IndexSet::default();
    diagnostics.insert(Diagnostic {
        level: Level::Error,
        messages: vec![Message {
            pos: Position {
                filename: filename.clone(),
                line: 1,
                column: None,
            },
            style: Style::Line,
            message: format!(
                "Cannot find the module {} from {}",
                "abc",
                root.clone() + "/abc"
            ),
            note: None,
        }],
        code: Some(DiagnosticId::Error(ErrorKind::CannotFindModule)),
    });
    diagnostics.insert(Diagnostic {
        level: Level::Warning,
        messages: vec![Message {
            pos: Position {
                filename: filename.clone(),
                line: 17,
                column: None,
            },
            style: Style::Line,
            message: format!("Importstmt should be placed at the top of the module"),
            note: Some("Consider moving tihs statement to the top of the file".to_string()),
        }],
        code: Some(DiagnosticId::Warning(WarningKind::ImportPositionWarning)),
    });
    diagnostics.insert(Diagnostic {
        level: Level::Warning,
        messages: vec![Message {
            pos: Position {
                filename: filename.clone(),
                line: 3,
                column: None,
            },
            style: Style::Line,
            message: format!("Module '{}' is reimported multiple times.", "a",),
            note: Some("Consider removing this statement".to_string()),
        }],
        code: Some(DiagnosticId::Warning(WarningKind::ReimportWarning)),
    });
    diagnostics.insert(Diagnostic {
        level: Level::Warning,
        messages: vec![Message {
            pos: Position {
                filename: filename.clone(),
                line: 1,
                column: None,
            },
            style: Style::Line,
            message: format!("Module '{}' imported but unused.", "abc",),
            note: Some("Consider removing this statement".to_string()),
        }],
        code: Some(DiagnosticId::Warning(WarningKind::UnusedImportWarning)),
    });

    for (d1, d2) in resolver.handler.diagnostics.iter().zip(diagnostics.iter()) {
        assert_eq!(d1, d2);
    }
}
