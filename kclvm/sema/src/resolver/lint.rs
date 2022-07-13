use indexmap::IndexSet;
use kclvm_error::{Diagnostic, DiagnosticId, Level, ErrorKind};
use kclvm_ast::ast;
use super::Resolver;

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
        $(#[inline(always)] fn $name(&mut self, $(_: $arg),*) {})*
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
            fn check_module(a: ast::Module);
            fn check_ident(a: ast::Identifier);
        ]);
    )
}

lint_methods!(declare_default_lint_pass_impl, []);

// pub trait LintPass{
//     fn name(&self) -> String;
//     fn get_lint() -> Lint;
//     fn check_ident(&mut self, a: ast::Identifier, diags: &mut IndexSet<Diagnostic>){}
//     fn check_module(&mut self, a: ast::Module, diags: &mut IndexSet<Diagnostic>){}
//     fn check_stmt(&mut self, a: ast::Stmt, diags: &mut IndexSet<Diagnostic>){}
// }

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

pub static Line_Too_Long: &Lint = &Lint {
    name: stringify!("LineTooLong"),
    level: Level::Warning,
    desc: "Line too long",
    code: "W501",
};

declare_lint_pass!(LineTooLong => [Line_Too_Long]);

impl DefaultLintPassImpl for LineTooLong{

}





// struct WhileTrue{
//     name: str
// }

// impl LintPass for WhileTrue{
//     fn name(&self) -> String{
//         self.name.to_string()
//     }

//     fn get_lint() -> Lint{
//         Lint {
//             name: "whiletrue",
//             level: Level::Warning,
//             desc: "whiletrue",
//             code: ErrorKind::AssertionError,
//         }
//     }

//     fn check_ident(&mut self, a: ast::Identifier, diags: &mut IndexSet<Diagnostic>){
//         println!("{:?}", a);
//     }
// }

impl Resolver<'_>{
    pub (crate) fn lint_check(&mut self){
        
    }
}