use indexmap::IndexSet;
use kclvm_error::{Diagnostic, DiagnosticId, Level, ErrorKind, WarningKind, Message, Position, Style};
use kclvm_ast::ast;
use super::Resolver;
use kclvm_ast::walker::MutSelfWalker;
pub struct Lint {
    /// A string identifier for the lint.
    pub name: &'static str,

    /// Level for the lint.
    pub level: Level,

    /// Description of the lint or the issue it detects.
    /// e.g., "imports that are never used"
    pub desc: &'static str,
    
    // Error/Warning code
    pub code: &'static str,
}



pub trait LintPass {
    fn name(&self) -> &'static str;
}

macro_rules! expand_default_lint_pass_methods {
    ([$($(#[$attr:meta])* fn $name:ident($($param:ident: $arg:ty),*);)*]) => (
        $(#[inline(always)] fn $name(&mut self, $($param: $arg),*) {})*
    )
}

macro_rules! declare_default_lint_pass_impl {
    ([], [$($methods:tt)*]) => (
        pub trait DefaultLintPassImpl: LintPass {
            expand_default_lint_pass_methods!([$($methods)*]);
        }
    )
}

#[macro_export]
macro_rules! lint_methods {
    ($macro:path, $args:tt) => (
        $macro!($args, [

            fn check_module(module: &ast::Module, diags: &mut IndexSet<Diagnostic>);
            /*
            * Stmt
            */

            // fn check_stmt(stmt: ast::Node<ast::Stmt>);
            // fn check_expr_stmt(expr_stmt: ast::ExprStmt);
            // fn check_unification_stmt(unification_stmt: ast::UnificationStmt);
            // fn check_type_alias_stmt(type_alias_stmt: ast::TypeAliasStmt);
            // fn check_assign_stmt(assign_stmt: ast::AssignStmt);
            // fn check_aug_assign_stmt(aug_assign_stmt: ast::AugAssignStmt);
            // fn check_assert_stmt(assert_stmt: ast::AssertStmt);
            // fn check_if_stmt(if_stmt: ast::IfStmt);
            // fn check_import_stmt(import_stmt: ast::ImportStmt);
            // fn check_schema_stmt(schema_stmt: ast::SchemaStmt);
            // fn check_rule_stmt(rule_stmt: ast::RuleStmt);

            /*
            * Expr
            */

            // fn check_expr(expr: ast::Node<ast::Expr>);
            // fn check_quant_expr(quant_expr: ast::QuantExpr);
            // fn check_schema_attr(schema_attr: ast::SchemaAttr);
            // fn check_if_expr(if_expr: ast::IfExpr);
            // fn check_unary_expr(unary_expr: ast::UnaryExpr);
            // fn check_binary_expr(binary_expr: ast::BinaryExpr);
            // fn check_selector_expr(selector_expr: ast::SelectorExpr);
            // fn check_call_expr(call_expr: ast::CallExpr);
            // fn check_subscript(subscript: ast::Subscript);
            // fn check_paren_expr(paren_expr: ast::ParenExpr);
            // fn check_list_expr(list_expr: ast::ListExpr);
            // fn check_list_comp(list_comp: ast::ListComp);
            // fn check_list_if_item_expr(list_if_item_expr: ast::ListIfItemExpr);
            // fn check_starred_expr(starred_expr: ast::StarredExpr);
            // fn check_dict_comp(dict_comp: ast::DictComp);
            // fn check_config_if_entry_expr(config_if_entry_expr: ast::ConfigIfEntryExpr,
            // );
            // fn check_comp_clause(comp_clause: ast::CompClause);
            // fn check_schema_expr(schema_expr: ast::SchemaExpr);
            // fn check_config_expr(config_expr: ast::ConfigExpr);
            // fn check_check_expr(check_expr: ast::CheckExpr);
            // fn check_lambda_expr(lambda_expr: ast::LambdaExpr);
            // fn check_keyword(keyword: ast::Keyword);
            // fn check_arguments(arguments: ast::Arguments);
            // fn check_compare(compare: ast::Compare);
            // fn check_identifier(identifier: ast::Identifier);
            // fn check_number_lit(number_lit: ast::NumberLit);
            // fn check_string_lit(string_lit: ast::StringLit);
            // fn check_name_constant_lit(name_constant_lit: ast::NameConstantLit);
            // fn check_joined_string(joined_string: ast::JoinedString);
            // fn check_formatted_value(formatted_value: ast::FormattedValue);
            // fn check_comment(comment: ast::Comment);
        ]);
    )
}

lint_methods!(declare_default_lint_pass_impl, []);

/// Declares a static `LintArray` and return it as an expression.
#[macro_export]
macro_rules! lint_array {
    ($( $lint:expr ),* ,) => { lint_array!( $($lint),* ) };
    ($( $lint:expr ),*) => {{
        vec![$($lint),*]
    }}
}

pub type LintArray = Vec<&'static Lint>;

#[macro_export]
macro_rules! declare_lint_pass {
    ($(#[$m:meta])* $name:ident => [$($lint:expr),* $(,)?]) => {
        $(#[$m])* #[derive(Copy, Clone)] pub struct $name;
        $crate::impl_lint_pass!($name => [$($lint),*]);
    };
}

/// Implements `LintPass for $ty` with the given list of `Lint` statics.
#[macro_export]
macro_rules! impl_lint_pass {
    ($ty:ty => [$($lint:expr),* $(,)?]) => {
        impl LintPass for $ty {
            fn name(&self) -> &'static str { stringify!($ty) }
        }
        impl $ty {
            pub fn get_lints() -> LintArray { $crate::lint_array!($($lint),*) }
        }
    };
}

pub static Import_Position: &Lint = &Lint {
    name: stringify!("ImportPosition"),
    level: Level::Warning,
    desc: "Importstmt should be at the top of file",
    code: "W401",
};

declare_lint_pass!(ImportPosition => [Import_Position]);

impl DefaultLintPassImpl for ImportPosition{
    fn check_module(&mut self, module: &ast::Module, diags: &mut IndexSet<Diagnostic>) {
        let mut firststmt = 2000;
        for stmt in &module.body{
            match &stmt.node{
                ast::Stmt::Import(_ImportStmt) => {}
                _ => {
                    if stmt.line < firststmt {
                        firststmt = stmt.line
                    }
                }
            }
        }
        for stmt in &module.body{
            if let ast::Stmt::Import(import_stmt) = &stmt.node {
                if stmt.line > firststmt {
                    diags.insert(
                        Diagnostic{
                            level: Level::Warning,
                            messages: (&[Message {
                                pos: Position {
                                    filename: module.filename.clone(),
                                    line: stmt.line,
                                    column: None,
                                },
                                style: Style::Line,
                                message: format!(
                                    "Importstmt should be at the top of file",
                                ),
                                note: None,
                            }]).to_vec(),
                            code: Some(DiagnosticId::Warning(WarningKind::ImportstmtPositionWarning))
                        }
                    );
                }
            }
        }
        
    }
}

pub struct Linter<T: LintPass>{
    pass: T,
    diags: IndexSet<Diagnostic>
}

impl Linter<ImportPosition> {
    pub fn new() -> Self{
        Linter::<ImportPosition> {
            pass: ImportPosition,
            diags: IndexSet::default()
        }

    }
}

#[macro_export]
macro_rules! walk_list {
    ($walker: expr, $method: ident, $list: expr) => {
        for elem in &$list {
            $walker.$method(&elem.node)
        }
    };
}

impl<'ctx> MutSelfWalker<'ctx> for Linter<ImportPosition>{
    fn walk_module(&mut self, module: &'ctx ast::Module){
        self.pass.check_module(module, &mut self.diags);
        walk_list!(self, walk_stmt, module.body)
    }
}

impl<'ctx> Resolver<'ctx> {
    pub fn lint_check(&mut self, pkgpath: &str){
        let mut linter = Linter::<ImportPosition>::new();
        match self.program.pkgs.get(pkgpath) {
            Some(modules) => {
                for module in modules {
                    linter.walk_module(module);
                }
            }
            None => {}
        }
        for diag in linter.diags{
            self.handler.diagnostics.insert(diag);
        }
    }
}