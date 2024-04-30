use crate::graph_memory_machine as gmm;
use wasm::{
    syntax::{Module, TypedFunctionImport, fn_type, TYPE_I32},
    base::{
        indices::{FunctionIndex},
        types::{FunctionType}
    },
};

#[derive(Debug)]
pub enum CompilationError {
}

pub type Result<A> = std::result::Result<A, CompilationError>;

fn import_runtime_function(module: &mut Module, fn_name: &str, type_: FunctionType) -> FunctionIndex {
    module.add_typed_function_import(TypedFunctionImport { module_name: "runtime".to_string(), name: fn_name.to_string(), type_ })
}

fn import_runtime(module: &mut Module,) -> Runtime {
    Runtime {
        const_: import_runtime_function(module, "const", fn_type(vec![TYPE_I32], vec![])),
        get_const: import_runtime_function(module, "get_const", fn_type(vec![], vec![TYPE_I32]) ),
        tuple: import_runtime_function(module, "tuple", fn_type(vec![TYPE_I32, TYPE_I32], vec![])),
        get_tuple_pointer: import_runtime_function(module, "get_tuple_pointer", fn_type(vec![], vec![TYPE_I32])),
        get_tuple_variant: import_runtime_function(module, "get_tuple_variant", fn_type(vec![], vec![TYPE_I32])),
        tuple_project: import_runtime_function(module, "tuple_project", fn_type(vec![TYPE_I32], vec![])),
        read_tag: import_runtime_function(module, "read_tag", fn_type(vec![], vec![TYPE_I32])),
        get_variant: import_runtime_function(module, "get_variant", fn_type(vec![], vec![TYPE_I32])),
        make_env: import_runtime_function(module, "make_env", fn_type(vec![TYPE_I32], vec![])),
        extend_env: import_runtime_function(module, "extend_env", fn_type(vec![TYPE_I32], vec![])),
        var: import_runtime_function(module, "var", fn_type(vec![TYPE_I32], vec![])),
        drop_env: import_runtime_function(module, "drop_env", fn_type(vec![], vec![])),
    }
}

#[derive(Debug)]
struct Runtime {
    const_: FunctionIndex,
    get_const: FunctionIndex,
    tuple: FunctionIndex,
    get_tuple_pointer: FunctionIndex,
    get_tuple_variant: FunctionIndex,
    tuple_project: FunctionIndex,
    read_tag: FunctionIndex,
    get_variant: FunctionIndex,
    make_env: FunctionIndex,
    extend_env: FunctionIndex,
    var: FunctionIndex,
    drop_env: FunctionIndex,
}

pub fn compile(program: gmm::Program) -> Result<Module> {
    let mut module = Module::empty();
    let runtime = import_runtime(&mut module);

    Ok(module)
}
