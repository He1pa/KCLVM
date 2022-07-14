use indexmap::{IndexSet, IndexMap};
use kclvm_error::{Diagnostic, DiagnosticId, Level, ErrorKind, WarningKind, Message, Position, Style};
use kclvm_ast::ast;
use regex::Regex;
use super::{Resolver, Context};
use kclvm_ast::walker::MutSelfWalker;

// 定义lint检查的步骤:
// 1. 定义 Lint -> pub static Import_Position: &Lint = &Lint {...}
// 2. 定义 lintpass -> declare_lint_pass!(ImportPosition => [Import_Position]); `=>`前为 LintPass，后为刚刚定义的 Lint
//    一个 lintpass 可以生成多个lint（一次检查发现多种 lint 错误）
// 3. 实现 lintpass检查，impl DefaultLintPassImpl for ImportPosition{...}
// 4. 将 lintpass 中的check_*方法添加到 lint_methods 宏中，如果有则跳过
// 5. 将新的 lintpass 添加到 default_lint_passes 宏中，注意 `:` 前后都是LintPass的名字
// 6. 如果第4步新增了 check_* 方法，则需要在遍历AST时调用，在 impl MutSelfWalker for Linter{...} 中重写 walk_* 方法。重写
//    时除了调用 self.pass.check_* 函数外，还要复制 MutSelfWalker 中原有的 walk 方法，使得能够继续遍历子节点。


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
            fn check_module(module: &ast::Module);
            fn check_module_post(module: &ast::Module);
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
            fn check_schema_attr(schema_attr: &ast::SchemaAttr);
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
            fn check_identifier(id: &ast::Identifier);
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

    // Error/Warning code
    pub note: Option<&'static str>,
}


// lint pass 定义
pub trait LintPass {
    fn name(&self) -> &'static str;
}


// DefaultLintPassImpl 定义，为每一个lintpass提供lint_methods中方法的默认实现: 进行空检查

macro_rules! expand_default_lint_pass_methods {
    ($diag:ty, $ctx:ty, [$($(#[$attr:meta])* fn $name:ident($($param:ident: $arg:ty),*);)*]) => (
        $(#[inline(always)] fn $name(&mut self, diags: &mut $diag, ctx: &mut $ctx, $($param: $arg),*) {})*
    )
}

macro_rules! declare_default_lint_pass_impl {
    ([], [$($methods:tt)*]) => (
        pub trait DefaultLintPassImpl: LintPass {
            expand_default_lint_pass_methods!(IndexSet<Diagnostic>, Context, [$($methods)*]);
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
    ($diags:ty, $ctx:ty, $passes:tt, [$($(#[$attr:meta])* fn $name:ident($($param:ident: $arg:ty),*);)*]) => (
        $(fn $name(&mut self, diags: &mut $diags, ctx: &mut $ctx, $($param: $arg),*) {
            expand_combined_lint_pass_method!($passes, self, $name, (diags, ctx, $($param),*));
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
            expand_combined_lint_pass_methods!(IndexSet<Diagnostic>, Context,[$($passes),*], $methods);
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
                UnusedImport: UnusedImport,
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
//     fn check_ident(&mut self, diags: &mut IndexSet<diagnostics>, ctx: &mut Context, id: &ast::Identifier, ){
//         self.LintPassA.check_ident(diags, ctx, id);
//         self.LintPassB.check_ident(diags, ctx, id);
//         ...
//     }
//     fn check_stmt(&mut self, diags: &mut IndexSet<diagnostics>, ctx: &mut Context, module: &ast::Module){
//         self.LintPassA.check_stmt(diags, ctx, module);
//         self.LintPassB.check_stmt(diags, ctx, module);
//         ...
//     }
// }
default_lint_passes!(declare_combined_default_pass, [CombinedLintPass]);



//--------linter--------------

pub struct Linter<'l, T: LintPass>{
    pass: T,
    diags: &'l mut IndexSet<Diagnostic>,
    ctx: &'l mut Context
}

impl<'l> Linter<'l, CombinedLintPass> {
    pub fn new(diags: &'l mut IndexSet<Diagnostic>, ctx: &'l mut Context) -> Self{
        Linter::<'l, CombinedLintPass> {
            pass: CombinedLintPass::new(),
            diags,
            ctx
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

#[macro_export]
macro_rules! walk_if {
    ($walker: expr, $method: ident, $value: expr) => {
        match &$value {
            Some(v) => $walker.$method(&v.node),
            None => (),
        }
    };
}

impl<'ctx> MutSelfWalker<'ctx> for Linter<'_, CombinedLintPass>{
    fn walk_module(&mut self, module: &'ctx ast::Module){
        self.pass.check_module(&mut self.diags, &mut self.ctx, module);
        walk_list!(self, walk_stmt, module.body);
        self.pass.check_module_post(&mut self.diags, &mut self.ctx, module);
    }

    fn walk_identifier(&mut self, id: &'ctx ast::Identifier){
        self.pass.check_identifier(&mut self.diags, &mut self.ctx, id);
    }

    fn walk_schema_attr(&mut self, schema_attr: &'ctx ast::SchemaAttr){
        self.pass.check_schema_attr(&mut self.diags, &mut self.ctx, schema_attr);
        walk_list!(self, walk_call_expr, schema_attr.decorators);
        walk_if!(self, walk_expr, schema_attr.value);
    }
}

impl<'ctx> Resolver<'ctx> {
    pub fn lint_check_module(&mut self, module: &ast::Module){
        let mut linter = Linter::<CombinedLintPass>::new(
            &mut self.handler.diagnostics,
            &mut self.ctx
        );
        linter.walk_module(module)
    }
}

// lint 定义 和 lintpass 的实现，

// importposition check
pub static Import_Position: &Lint = &Lint {
    name: stringify!("Import_Position"),
    level: Level::Warning,
    desc: "Check for importstmt that are not defined at the top of file",
    code: "W0413",
    note: Some("Consider moving tihs statement to the top of the file")
};

declare_lint_pass!(ImportPosition => [Import_Position]);

impl DefaultLintPassImpl for ImportPosition{
    fn check_module(&mut self, diags: &mut IndexSet<Diagnostic>, ctx: &mut Context, module: &ast::Module,) {
        let mut first_non_importstmt = std::u64::MAX;
        for stmt in &module.body{
            match &stmt.node{
                ast::Stmt::Import(_import_stmt) => {}
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
                    let lint = ImportPosition::get_lints()[0];
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
                                    "Importstmt should be placed at the top of the module"
                                ),
                                note: Some(lint.note.unwrap().clone().to_string()),
                            }]).to_vec(),
                            code: Some(DiagnosticId::Warning(WarningKind::ImportstmtPositionWarning))
                        }
                    );
                }
            }
        }
    }
}

// unusedimport check
pub static Unused_Import: &Lint = &Lint {
    name: stringify!("Unused_Import"),
    level: Level::Warning,
    desc: "Check for unused importstmt",
    code: "W0411",
    note: Some("Consider removing this importstmt")
};

declare_lint_pass!(UnusedImport => [Unused_Import]);

fn record_use(name: &String, ctx: &mut Context) {
    let re = Regex::new(r"[|:\[\]\{\}]").unwrap();
    // # SchemaAttr.types, A|B, [A|B], {A|B:C}
    let types: Vec<&str> = re.split(name).collect();
    for t in types {
        let t = t.to_string();
        // name: a.b.c
        let name: Vec<&str> = t.split(".").collect();
        let firstname = name[0];
        if let Some(import_names) = ctx.import_names.get(&ctx.filename){
            if import_names.contains_key(firstname) {
                ctx
                .used_import_names
                .get_mut(&ctx.filename)
                .unwrap()
                .insert(firstname.to_string());
            }
        }
    }
}

impl DefaultLintPassImpl for UnusedImport{
    fn check_module(&mut self, diags: &mut IndexSet<Diagnostic>, ctx: &mut Context, module: &ast::Module) {
        println!("{:?}", &ctx.filename);
        ctx
        .used_import_names
        .insert(ctx.filename.clone(), IndexSet::default());
    }

    fn check_module_post(&mut self, diags: &mut IndexSet<Diagnostic>, ctx: &mut Context, module: &ast::Module) {
        let used_import_names = ctx.used_import_names.get(&ctx.filename).unwrap();
        println!("{:?}", used_import_names);
        for stmt in &module.body {
            if let ast::Stmt::Import(import_stmt) = &stmt.node {
                if !used_import_names.contains(&import_stmt.name) {
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
                                    "Module '{}' imported but unused.",
                                    import_stmt.name
                                ),
                                note: None
                            }]).to_vec(),
                            code: Some(DiagnosticId::Warning(WarningKind::UnusedImportWarning))
                        }
                    );
                }
            }
        }
    }

    fn check_identifier(&mut self, diags: &mut IndexSet<Diagnostic>, ctx: &mut Context, id: &ast::Identifier){
        println!("{:?}", id.names);
        if id.names.len() >= 2 {
            let id_firstname = &id.names[0];
            record_use(&id_firstname, ctx);
        }
    }

    fn check_schema_attr(&mut self, diags: &mut IndexSet<Diagnostic>, ctx: &mut Context, schema_attr: &ast::SchemaAttr){
        record_use(&schema_attr.type_str.node, ctx);
    }
}
