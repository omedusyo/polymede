use crate::expr::Expr;
use crate::parser;
use crate::parser::parse;

use wasm::base::export::{Export, ExportDescription};
use wasm::syntax::{
    fn_type, i32_add, i32_const, i32_mul, Expression, Module, TypedFunction, TYPE_I32,
};

pub fn compile(expr: Expr) -> Expression {
    use Expr::*;
    match expr {
        Nat32(_, x) => i32_const(x as i32),
        Add(_, e0, e1) => i32_add(compile(*e0), compile(*e1)),
        Mul(_, e0, e1) => i32_mul(compile(*e0), compile(*e1)),
    }
}

pub fn compile_from_str(str: &str) -> Result<Module, parser::Error> {
    let expr = parse(str)?;
    let compiled_expr: Expression = compile(expr);

    let mut module = Module::empty();

    let value = module.add_typed_function(TypedFunction {
        type_: fn_type(vec![], vec![TYPE_I32]),
        locals: vec![],
        body: compiled_expr,
    });

    module.add_export(Export {
        name: "value".to_string(),
        export_description: ExportDescription::Function(value),
    });

    Ok(module)
}
