use criterion::{criterion_group, criterion_main, Criterion};
use kclvm_parser::load_program;
use kclvm_sema::pre_process::pre_process_program;
use kclvm_sema::ty::*;
use kclvm_sema::resolver::*;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("sup", |b| {
        b.iter(|| {
            // let types = vec![
            //     Type::int_lit(1),
            //     Type::INT,
            //     Type::union(&[Type::STR, Type::dict(Type::STR, Type::STR)]),
            //     Type::dict(Type::ANY, Type::ANY),
            // ];
            // sup(&types);
        })
    });
}

pub fn criterion_benchmark_resolver(c: &mut Criterion) {
    let mut program = load_program(&["./src/resolver/test_data/import.k"], None).unwrap();
    c.bench_function("resolver", |b| {
        b.iter(|| {
            resolve_program(&mut program)
        })
    });
}

pub fn criterion_benchmark_unusedimportcheck(c: &mut Criterion) {
    let mut program = load_program(&["./src/resolver/test_data/import.k"], None).unwrap();
    pre_process_program(&mut program);
    let mut resolver = Resolver::new(
        &program,
        Options {
            raise_err: true,
            config_auto_fix: false,
        },
    );
    resolver.resolve_import();
    

    resolver.ctx.pkgpath = kclvm_ast::MAIN_PKG.to_string();
    let filename = resolver.ctx.filename.clone();
    resolver.change_package_context(kclvm_ast::MAIN_PKG, &filename);
    resolver.init_import_list();



    c.bench_function("unused_import_check", |b| {
        b.iter(|| {
            resolver.check_unused_import(kclvm_ast::MAIN_PKG)
        })
    });
}

criterion_group!(benches, criterion_benchmark, criterion_benchmark_resolver, criterion_benchmark_unusedimportcheck);
criterion_main!(benches);
