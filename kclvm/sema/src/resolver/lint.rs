use indexmap::IndexSet;
use kclvm_error::{Diagnostic, DiagnosticId, Level, ErrorKind, WarningKind, Message, Position, Style};
use kclvm_ast::ast;
use super::Resolver;
use kclvm_ast::walker::MutSelfWalker;

// 定义lint检查的步骤:
// 1. 定义 Lint -> pub static Import_Position: &Lint = &Lint {...}
// 2. 定义 lintpass -> declare_lint_pass!(ImportPosition => [Import_Position]); `=>`前为 LintPass，后为刚刚定义的 Lint
//    一个 lintpass 可以生成多个lint（一次检查发现多种 lint 错误）
// 3. 实现 lintpass检查，impl DefaultLintPassImpl for ImportPosition{...}
// 4. 将 lintpass 中的check_*方法添加到 lint_methods 宏中，如果有则跳过
// 5. 将新的 lintpass 添加到 default_lint_passes 宏中，注意 `:` 前后都是LintPass的名字
// 6. 如果第4步新增了 check_* 方法，则需要在遍历AST时调用，在 impl MutSelfWalker for Linter{...} 中重写 walk_* 方法。重写
//    时除了调用 self.pass.check_* 函数外，还要复制 MutSelfWalker 中原有的 walk 方法，使得能够继续遍历。


pub type LintArray = Vec<&'static Lint>;

/// Declares a static `LintArray` and return it as an expression.
#[macro_export]
macro_rules! lint_array {
    ($( $lint:expr ),* ,) => { lint_array!( $($lint),* ) };
    ($( $lint:expr ),*) => {{
        vec![$($lint),*]
    }}
}

// lintpass 中需要实现的方法的汇总，新增lint和lintpass时，按需补充
// DefaultLintPassImpl 中有这些方法的默认实现（空检查），所以lintpass也只需要实现自己所需要的检查函数
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

// lint 定义
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


// lint pass 定义
pub trait LintPass {
    fn name(&self) -> &'static str;
}


// DefaultLintPassImpl 定义，为每一个lintpass提供lint_methods中方法的默认实现: 进行空检查

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

lint_methods!(declare_default_lint_pass_impl, []);

// 定义lintpass的宏，并绑定一组对应的lint
#[macro_export]
macro_rules! declare_lint_pass {
    ($(#[$m:meta])* $name:ident => [$($lint:expr),* $(,)?]) => {
        $(#[$m])* #[derive(Copy, Clone)] pub struct $name;
        $crate::impl_lint_pass!($name => [$($lint),*]);
    };
}

// 为lintpass实现lint中的fn name() 和 get_lints()方法
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

// 定义 CombinedLintPass 的宏
#[macro_export]
macro_rules! expand_combined_lint_pass_method {
    ([$($passes:ident),*], $self: ident, $name: ident, $params:tt) => ({
        $($self.$passes.$name $params;)*
    })
}

#[macro_export]
macro_rules! expand_combined_lint_pass_methods {
    ($passes:tt, [$($(#[$attr:meta])* fn $name:ident($($param:ident: $arg:ty),*);)*]) => (
        $(fn $name(&mut self, $($param: $arg),*) {
            expand_combined_lint_pass_method!($passes, self, $name, ($($param),*));
        })*
    )
}

#[macro_export]
macro_rules! declare_combined_lint_pass {
    ([$v:vis $name:ident, [$($passes:ident: $constructor:expr,)*]], $methods:tt) => (
        #[allow(non_snake_case)]
        $v struct $name {
            $($passes: $passes,)*
        }

        impl $name {
            $v fn new() -> Self {
                Self {
                    $($passes: $constructor,)*
                }
            }

            $v fn get_lints() -> LintArray {
                let mut lints = Vec::new();
                $(lints.extend_from_slice(&$passes::get_lints());)*
                lints
            }
        }

        impl DefaultLintPassImpl for $name {
            expand_combined_lint_pass_methods!([$($passes),*], $methods);
        }

        // #[allow(rustc::lint_pass_impl_without_macro)]
        impl LintPass for $name {
            fn name(&self) -> &'static str {
                // todo
                panic!()
            }
        }
    )
}


macro_rules! default_lint_passes {
    ($macro:path, $args:tt) => {
        $macro!(
            $args,
            [
                ImportPosition: ImportPosition,
            ]
        );
    };
}

macro_rules! declare_combined_default_pass {
    ([$name:ident], $passes:tt) => (
        lint_methods!(declare_combined_lint_pass, [pub $name, $passes]);
    )
}

// combined lintpass 定义. 从default_lint_passes宏开始，将所有默认的lintpass的检查方法汇总到CombinedLintPass中
// 最终生成的代码如：
// pub struct CombinedLintPass {
//     LintPassA: LintPassA;
//     LintPassB: LintPassB;
//     ...
// }
//
// impl CombinedLintPass{
//     pub fn new() -> CombinedLintPass { 
//        CombinedLintPass {
//            LintPassA: LintPassA,
//            LintPassB: LintPassB,
//            ...
//        } 
//     }
//     pub fn get_lints() -> LintArray {
//         let mut lints = Vec::new();
//         lints.extend_from_slice(&LintPassA::get_lints());
//         lints.extend_from_slice(&LintPassB::get_lints());
//         ...
//         lints
//      }    
//  }
//
// impl DefaultLintPassImpl for CombinedLintPass {
//     fn check_ident(&mut self, a: Ident, &mut diags: IndexSet<diagnostics>){
//         self.LintPassA.check_ident(a, diags);
//         self.LintPassB.check_ident(a, diags);
//         ...
//     }
//     fn check_stmt(&mut self, a: &ast::Stmt, &mut diags: IndexSet<diagnostics>){
//         self.LintPassA.check_stmt(a, diags);
//         self.LintPassB.check_stmt(a, diags);
//         ...
//     }
/// }
default_lint_passes!(declare_combined_default_pass, [CombinedLintPass]);


// 具体的lint 和lintpass 定义，

// importposition check
pub static Import_Position: &Lint = &Lint {
    name: stringify!("ImportPosition"),
    level: Level::Warning,
    desc: "Importstmt should be at the top of file",
    code: "W401",
};

declare_lint_pass!(ImportPosition => [Import_Position]);

impl DefaultLintPassImpl for ImportPosition{
    fn check_module(&mut self, module: &ast::Module, diags: &mut IndexSet<Diagnostic>) {
        let mut first_non_importstmt = 2000;
        for stmt in &module.body{
            match &stmt.node{
                ast::Stmt::Import(_ImportStmt) => {}
                _ => {
                    if stmt.line < first_non_importstmt {
                        first_non_importstmt = stmt.line
                    }
                }
            }
        }
        for stmt in &module.body{
            if let ast::Stmt::Import(_import_stmt) = &stmt.node {
                if stmt.line > first_non_importstmt {
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



//--------linter--------------

pub struct Linter<'l, T: LintPass>{
    pass: T,
    diags: &'l mut IndexSet<Diagnostic>
}

impl<'l> Linter<'l, CombinedLintPass> {
    pub fn new(diags: &'l mut IndexSet<Diagnostic>) -> Self{
        Linter::<'l, CombinedLintPass> {
            pass: CombinedLintPass::new(),
            diags
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



impl<'ctx> MutSelfWalker<'ctx> for Linter<'_, CombinedLintPass>{
    fn walk_module(&mut self, module: &'ctx ast::Module){
        self.pass.check_module(module, &mut self.diags);
        walk_list!(self, walk_stmt, module.body)
    }
}

impl<'ctx> Resolver<'ctx> {
    pub fn lint_check(&mut self, pkgpath: &str){
        let mut linter = Linter::<CombinedLintPass>::new(&mut self.handler.diagnostics);
        match self.program.pkgs.get(pkgpath) {
            Some(modules) => {
                for module in modules {
                    linter.walk_module(module);
                }
            }
            None => {}
        }
    }
}