use crate::graph_memory_machine as gmm;
use wasm::{
    syntax::{Module, TypedFunctionImport, TypedFunction, fn_type, TYPE_I32, Expression, call, call_indirect, i32_const, i32_add, local_get, i32_eq, seq},
    base::{
        indices::{FunctionIndex, TableIndex, TypeIndex},
        types::{FunctionType, BlockType, ValueType, NumType},
        export::{Export, ExportDescription},
    },
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilationError {
    NumberOfPrimitiveFunctionsMismatch { number_of_primitive_functions_assumed_in_program: usize, number_of_primitive_functions_in_library: usize },
    VariableOutOfBounds { var_index_received: gmm::VarName, number_of_parameters: usize },
    FunctionOutOfBounds { fn_name_received: gmm::FunctionName, number_of_all_functions_available_in_program: usize },
}

pub type Result<A> = std::result::Result<A, CompilationError>;

type FunctionTableIndex = i32;

#[derive(Debug)]
struct State {
    gmm_to_wasm_name_map: HashMap<gmm::FunctionName, FunctionInfo>,

    runtime: Runtime,

    function_type_for_call_indirect: TypeIndex,
    function_table_map: HashMap<FunctionIndex, FunctionTableIndex>,
    function_table: Vec<FunctionIndex>,
}

#[derive(Debug, Copy, Clone)]
struct FunctionInfo {
    function_index: FunctionIndex,
    kind: FunctionKind,
}

#[derive(Debug, Copy, Clone)]
enum FunctionKind {
    UserDefined,
    Primitive,
}

#[derive(Debug)]
struct Runtime {
    number_of_runtime_functions: usize,
    const_: FunctionIndex,
    get_const: FunctionIndex,
    tuple: FunctionIndex,
    get_tuple_pointer: FunctionIndex,
    get_tuple_variant: FunctionIndex,
    tuple_project: FunctionIndex,
    read_tag: FunctionIndex,
    get_variant: FunctionIndex,
    make_env: FunctionIndex,
    make_env_from: FunctionIndex,
    copy_and_extend_env: FunctionIndex,
    extend_env: FunctionIndex,
    var: FunctionIndex,
    drop_env: FunctionIndex,
    partial_apply: FunctionIndex,
    make_env_from_closure: FunctionIndex,
}

impl State {
    fn new(module: &mut Module, runtime: Runtime) -> Self {
        let function_type_for_call_indirect = module.add_function_type(FunctionType { domain: vec![], codomain: vec![] });
        Self {
            gmm_to_wasm_name_map: HashMap::new(),

            function_type_for_call_indirect,
            function_table_map: HashMap::new(),
            function_table: vec![],

            runtime,
        }
    }

    fn function_info(&self, fn_name: gmm::FunctionName) -> Result<FunctionInfo> {
        match self.gmm_to_wasm_name_map.get(&fn_name) {
            Some(fn_info) => Ok(*fn_info),
            None => Err(CompilationError::FunctionOutOfBounds {
                fn_name_received: fn_name,
                number_of_all_functions_available_in_program: self.runtime.number_of_runtime_functions + self.gmm_to_wasm_name_map.len()
            })
        }
    }

    fn wasm_function_index(&self, fn_name: gmm::FunctionName) -> Result<FunctionIndex> {
        let fn_info = self.function_info(fn_name)?;
        Ok(fn_info.function_index)
    }

    fn register_function_in_function_table(&mut self, fn_name: gmm::FunctionName) -> Result<FunctionTableIndex> {
        let fn_index = self.wasm_function_index(fn_name)?;
        match self.function_table_map.get(&fn_index) {
            Some(fn_table_index) => Ok(*fn_table_index),
            None => {
                let fn_table_index = self.function_table.len() as i32;
                self.function_table_map.insert(fn_index, fn_table_index);
                self.function_table.push(fn_index);
                Ok(fn_table_index)
            }
        }
    }
}

// ===Imports===
fn import_runtime_function(module: &mut Module, fn_name: &str, type_: FunctionType) -> FunctionIndex {
    module.add_typed_function_import(TypedFunctionImport { module_name: "runtime".to_string(), name: fn_name.to_string(), type_ })
}

fn import_primitive_function(module: &mut Module, external_name: String) -> FunctionIndex {
    let type_ = FunctionType { domain: vec![], codomain: vec![] };
    module.add_typed_function_import(TypedFunctionImport { module_name: "primitives".to_string(), name: external_name, type_ })
}

fn import_runtime(module: &mut Module) -> Runtime {
    let mut number_of_runtime_functions = 0;
    fn import(module: &mut Module, fn_name: &str, type_: FunctionType, number_of_runtime_functions: &mut usize) -> FunctionIndex {
        let fn_index = import_runtime_function(module, fn_name, type_);
        *number_of_runtime_functions += 1;
        fn_index
    }

    let const_= import(module, "const", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let get_const = import(module, "get_const", fn_type(vec![], vec![TYPE_I32]) , &mut number_of_runtime_functions);
    let tuple = import(module, "tuple", fn_type(vec![TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let get_tuple_pointer = import(module, "get_tuple_pointer", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
    let get_tuple_variant = import(module, "get_tuple_variant", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
    let tuple_project = import(module, "tuple_project", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let read_tag = import(module, "read_tag", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
    let get_variant = import(module, "get_variant", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
    let make_env = import(module, "make_env", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let make_env_from = import(module, "make_env_from", fn_type(vec![TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let copy_and_extend_env = import(module, "extend_env", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let extend_env = import(module, "extend_env", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let var = import(module, "var", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let drop_env = import(module, "drop_env", fn_type(vec![], vec![]), &mut number_of_runtime_functions);
    let partial_apply = import(module, "partial_apply", fn_type(vec![TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);
    let make_env_from_closure = import(module, "make_env_from_closure", fn_type(vec![TYPE_I32], vec![TYPE_I32]), &mut number_of_runtime_functions);

    Runtime {
        number_of_runtime_functions,
        const_,
        get_const,
        tuple,
        get_tuple_pointer,
        get_tuple_variant,
        tuple_project,
        read_tag,
        get_variant,
        make_env,
        make_env_from,
        copy_and_extend_env,
        extend_env,
        var,
        drop_env,
        partial_apply,
        make_env_from_closure,
    }
}

// ===Compilation===
pub fn compile(program: gmm::Program) -> Result<Module> {
    let mut module = Module::empty();
    let runtime = import_runtime(&mut module);
    let mut state = State::new(&mut module, runtime);

    // ===Split program.functions into two separate vectors of primitive imports and user-defined functions===
    let mut gmm_imports: Vec<(gmm::FunctionName, gmm::FunctionImport)> = vec![];
    let mut gmm_functions: Vec<(gmm::FunctionName, gmm::Function)> = vec![];
    for (next_gmm_function_name, function) in program.functions.into_iter().enumerate() {
        match function {
            gmm::FunctionOrImport::Fn(function) => {
                gmm_functions.push((next_gmm_function_name, function))
            },
            gmm::FunctionOrImport::Import(import) => {
                gmm_imports.push((next_gmm_function_name, import))
            }
        }
    }

    // ===Setup `state.gmm_to_wasm_name_map`===
    {
        let mut next_wasm_function_index = state.runtime.number_of_runtime_functions;
        for (gmm_function_name, _) in &gmm_imports {
            state.gmm_to_wasm_name_map.insert(*gmm_function_name, FunctionInfo { function_index: FunctionIndex(next_wasm_function_index as u32), kind: FunctionKind::Primitive });
            next_wasm_function_index += 1;
        }

        for (gmm_function_name, _) in &gmm_functions {
            state.gmm_to_wasm_name_map.insert(*gmm_function_name, FunctionInfo { function_index: FunctionIndex(next_wasm_function_index as u32), kind: FunctionKind::UserDefined });
            next_wasm_function_index += 1;
        }
    }

    // Compile imports.
    for (_, gmm_import) in gmm_imports {
        import_primitive_function(&mut module, gmm_import.external_name);
    }
    // Compile user-defined functions.
    for (_, function) in gmm_functions {
        module.add_typed_function(compile_function(&mut state, function.number_of_parameters, &function.body)?);
    }

    // Compile main.
    let main = module.add_typed_function(compile_function(&mut state, 0, &program.main)?);
    module.add_export(Export { name: "main".to_string(), export_description: ExportDescription::Function(main) });

    module.register_function_table(state.function_table);

    Ok(module)
}

fn compile_function(state: &mut State, number_of_parameters: usize, body: &gmm::Term) -> Result<TypedFunction> {
    Ok(TypedFunction {
        type_: fn_type(vec![], vec![]),
        locals: vec![],
        body: compile_term(state, number_of_parameters, body)?,
    })
}

fn compile_terms(state: &mut State, number_of_parameters: usize, terms: &[gmm::Term]) -> Result<Vec<Expression>> {
    let mut args: Vec<Expression> = Vec::with_capacity(terms.len());
    for term in terms {
        let expression = compile_term(state, number_of_parameters, term)?;
        args.push(expression);
    }
    Ok(args)
}

fn compile_term(state: &mut State, number_of_parameters: usize, term: &gmm::Term) -> Result<Expression> {
    match term {
        gmm::Term::Const(variant) => {
            Ok(call(state.runtime.const_, vec![i32_const(*variant)]))
        },
        gmm::Term::ByteArray(bytes) => {
            todo!()
        },
        gmm::Term::Tuple(variant, terms) => {
            let mut args: Vec<Expression> = compile_terms(state, number_of_parameters, terms)?;
            // (call $tuple $variant $number_of_components)
            args.push(call(state.runtime.tuple, vec![i32_const(*variant), i32_const(terms.len() as i32)]));
            Ok(seq(args))
        },
        gmm::Term::ProjectComponent(term, component_index) => {
            Ok(seq(
                vec![compile_term(state, number_of_parameters, term)?,
                call(state.runtime.tuple_project, vec![i32_const(*component_index as i32)]),
            ]))
        },
        gmm::Term::Call(fn_name, terms) => {
            let FunctionInfo { function_index, kind } = state.function_info(*fn_name)?;
            let mut args: Vec<Expression> = compile_terms(state, number_of_parameters, terms)?;
            match kind {
                FunctionKind::Primitive => {
                    args.push(call(function_index, vec![]));
                    Ok(seq(args))
                },
                FunctionKind::UserDefined => {
                    // (call $make_env (i32.const $number_of_arguments))
                    // (call $fn_name)
                    // (call $drop_env)
                    args.push(call(state.runtime.make_env, vec![i32_const(terms.len() as i32)]));
                    args.push(call(function_index, vec![]));
                    args.push(call(state.runtime.drop_env, vec![]));
                    Ok(seq(args))
                },
            }
        },
        gmm::Term::PartialApply(fn_name, terms) => {
            let mut code: Vec<Expression> = compile_terms(state, number_of_parameters, terms)?;
            let fn_table_index: FunctionTableIndex = state.register_function_in_function_table(*fn_name)?;
            code.push(call(state.runtime.partial_apply, vec![i32_const(fn_table_index), i32_const(terms.len() as i32)]));
            Ok(seq(code))
        },
        gmm::Term::CallClosure(closure_term, terms) => {
            let mut code: Vec<Expression> = vec![compile_term(state, number_of_parameters, closure_term)?];
            for compiled_term in compile_terms(state, number_of_parameters, terms)? {
                code.push(compiled_term)
            }
            code.push(call_indirect(state.function_type_for_call_indirect, TableIndex(0), vec![call(state.runtime.make_env_from_closure, vec![i32_const(terms.len() as i32)])]));
            code.push(call(state.runtime.drop_env, vec![]));
            Ok(seq(code))
        },
        gmm::Term::VarUse(var_name) => {
            if *var_name < number_of_parameters {
                Ok(call(state.runtime.var, vec![i32_const(*var_name as i32)]))
            } else {
                Err(CompilationError::VariableOutOfBounds { var_index_received: *var_name, number_of_parameters })
            }
        },
        gmm::Term::Let(terms, body_term) => {
            let mut code = vec![];
            let mut number_of_parameters = number_of_parameters;
            code.push(call(state.runtime.copy_and_extend_env, vec![i32_const(0)]));
            for term in terms {
                code.push(compile_term(state, number_of_parameters, term)?);
                number_of_parameters += 1;
                code.push(call(state.runtime.extend_env, vec![i32_const(1)]));
            }
            code.push(compile_term(state, number_of_parameters, body_term)?);
            code.push(call(state.runtime.drop_env, vec![]));
            Ok(seq(code))
        },
        gmm::Term::Match(arg_term, branches) => {
            let mut code = vec![];
            code.push(compile_term(state, number_of_parameters, arg_term)?);
            code.push(call(state.runtime.copy_and_extend_env, vec![i32_const(1)]));
            let arg_index = number_of_parameters;
            // TODO: We extend the environment with the argument, which currently is extremely
            // inefficient.
            code.push(compile_branches(state, number_of_parameters + 1, arg_index as i32, branches)?);
            code.push(call(state.runtime.drop_env, vec![]));
            Ok(seq(code))
        },
        gmm::Term::Seq(terms) => {
            // TODO: I think I need to introduce am explicit pop instruction for the stack.
            todo!()
        },
    }
}

fn compile_branches(state: &mut State, number_of_parameters: usize, arg_index: i32, branches: &[(gmm::Pattern, gmm::Term)]) -> Result<Expression> {
    if branches.is_empty() {
        Ok(Expression::Unreachable)
    } else {
        let (pattern, body) = &branches[0];
        match pattern {
            gmm::Pattern::Variant(variant) => {
                Ok(Expression::IfThenElse {
                    type_: BlockType::EmptyType,
                    test: Box::new(seq(vec![
                            call(state.runtime.var, vec![i32_const(arg_index)]),
                            i32_eq(call(state.runtime.get_variant, vec![]), i32_const(*variant)),
                    ])),
                    then_body: Box::new(compile_term(state, number_of_parameters, body)?),
                    else_body: Box::new(compile_branches(state, number_of_parameters, arg_index, &branches[1..])?),
                })
            },
            gmm::Pattern::Always => {
                compile_term(state, number_of_parameters, body)
            }
        }
    }
}
