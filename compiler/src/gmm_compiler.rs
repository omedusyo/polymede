use crate::graph_memory_machine as gmm;
use wasm::{
    syntax::{Module, TypedFunctionImport, TypedFunction, fn_type, TYPE_I32, Expression, call, i32_const, i32_add, local_get, seq},
    base::{
        indices::{FunctionIndex},
        types::{FunctionType},
        export::{Export, ExportDescription},
    },
};

#[derive(Debug)]
pub enum CompilationError {
    NumberOfPrimitiveFunctionsMismatch { number_of_primitive_functions_assumed_in_program: usize, number_of_primitive_functions_in_library: usize },
    VariableOutOfBounds { var_index_received: gmm::VarName, number_of_parameters: usize },
    FunctionOutOfBounds { fn_name_received: gmm::FunctionName, number_of_all_functions_available_in_program: usize },
}

pub type Result<A> = std::result::Result<A, CompilationError>;

#[derive(Debug)]
struct Runtime {
    number_of_user_defined_functions: usize,
    number_of_primitive_functions: usize,

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
    copy_and_extend_env: FunctionIndex,
    extend_env: FunctionIndex,
    var: FunctionIndex,
    drop_env: FunctionIndex,
}

pub struct PrimitiveFunctions {
    pub number_of_primitives: usize,
    pub add: gmm::FunctionName,
    pub inc: gmm::FunctionName,
    pub dec: gmm::FunctionName,
}

impl PrimitiveFunctions {
    pub fn standard() -> PrimitiveFunctions {
        Self {
            number_of_primitives: 3,

            // This has to match with the order of primitive functions in the `runtime.wat`
            add: 0,
            inc: 1,
            dec: 2,
        }
    }
}

impl Runtime {
    fn number_of_all_functions_available_in_program(&self) -> usize {
        // Note that we've excluded `self.number_of_runtime_functions`
        self.number_of_primitive_functions + self.number_of_user_defined_functions
    }

    fn to_wasm_function_index(&self, fn_name: gmm::FunctionName) -> FunctionIndex {
        FunctionIndex((self.number_of_runtime_functions + fn_name) as u32)
    }

    fn is_primitive(&self, fn_name: gmm::FunctionName) -> bool {
        fn_name < self.number_of_primitive_functions
    }

    fn is_user_defined(&self, fn_name: gmm::FunctionName) -> bool {
        self.number_of_primitive_functions <= fn_name && fn_name < self.number_of_primitive_functions + self.number_of_user_defined_functions
    }
}

fn import_runtime_function(module: &mut Module, fn_name: &str, type_: FunctionType) -> FunctionIndex {
    module.add_typed_function_import(TypedFunctionImport { module_name: "runtime".to_string(), name: fn_name.to_string(), type_ })
}

fn import_runtime(module: &mut Module, program: &gmm::Program, primitives: PrimitiveFunctions) -> Runtime {
    let runtime = Runtime {
        number_of_user_defined_functions: program.functions.len(),
        number_of_primitive_functions: primitives.number_of_primitives,

        number_of_runtime_functions: 12,
        const_: import_runtime_function(module, "const", fn_type(vec![TYPE_I32], vec![])),
        get_const: import_runtime_function(module, "get_const", fn_type(vec![], vec![TYPE_I32]) ),
        tuple: import_runtime_function(module, "tuple", fn_type(vec![TYPE_I32, TYPE_I32], vec![])),
        get_tuple_pointer: import_runtime_function(module, "get_tuple_pointer", fn_type(vec![], vec![TYPE_I32])),
        get_tuple_variant: import_runtime_function(module, "get_tuple_variant", fn_type(vec![], vec![TYPE_I32])),
        tuple_project: import_runtime_function(module, "tuple_project", fn_type(vec![TYPE_I32], vec![])),
        read_tag: import_runtime_function(module, "read_tag", fn_type(vec![], vec![TYPE_I32])),
        get_variant: import_runtime_function(module, "get_variant", fn_type(vec![], vec![TYPE_I32])),
        make_env: import_runtime_function(module, "make_env", fn_type(vec![TYPE_I32], vec![])),
        copy_and_extend_env: import_runtime_function(module, "extend_env", fn_type(vec![TYPE_I32], vec![])),
        extend_env: import_runtime_function(module, "extend_env", fn_type(vec![], vec![])),
        var: import_runtime_function(module, "var", fn_type(vec![TYPE_I32], vec![])),
        drop_env: import_runtime_function(module, "drop_env", fn_type(vec![], vec![])),
    };

    import_runtime_function(module, "add", fn_type(vec![], vec![]));
    import_runtime_function(module, "inc", fn_type(vec![], vec![]));
    import_runtime_function(module, "dec", fn_type(vec![], vec![]));

    runtime
}

pub struct PrimitiveFunctionNames {
    pub inc: gmm::FunctionName,
}

fn compile_terms(runtime: &Runtime, number_of_parameters: usize, terms: &[gmm::Term]) -> Result<Vec<Expression>> {
    let mut args: Vec<Expression> = Vec::with_capacity(terms.len());
    for term in terms {
        let expression = compile_term(runtime, number_of_parameters, term)?;
        args.push(expression);
    }
    Ok(args)
}

fn compile_term(runtime: &Runtime, number_of_parameters: usize, term: &gmm::Term) -> Result<Expression> {
    use Expression::*;
    match term {
        gmm::Term::Const(variant) => {
            Ok(call(runtime.const_, vec![i32_const(*variant)]))
        },
        gmm::Term::ByteArray(bytes) => {
            todo!()
        },
        gmm::Term::Tuple(variant, terms) => {
            let mut args: Vec<Expression> = compile_terms(runtime, number_of_parameters, terms)?;
            // (call $tuple $variant $number_of_components)
            args.push(call(runtime.tuple, vec![i32_const(*variant), i32_const(terms.len() as i32)]));
            Ok(seq(args))
        },
        gmm::Term::ProjectComponent(term, component_index) => {
            Ok(seq(
                vec![compile_term(runtime, number_of_parameters, term)?,
                call(runtime.tuple_project, vec![i32_const(*component_index as i32)]),
            ]))
        },
        gmm::Term::Call(fn_name, terms) => {
            if runtime.is_primitive(*fn_name) {
                let mut args: Vec<Expression> = compile_terms(runtime, number_of_parameters, terms)?;
                args.push(call(runtime.to_wasm_function_index(*fn_name), vec![]));
                Ok(seq(args))
            } else if runtime.is_user_defined(*fn_name) {
                println!("INDEE WE ARE CALLING USER DEF FUNCTION");
                let mut args: Vec<Expression> = compile_terms(runtime, number_of_parameters, terms)?;
                // (call $make_env (i32.const $number_of_arguments))
                // (call $fn_name)
                // (call $drop_env)
                args.push(call(runtime.make_env, vec![i32_const(terms.len() as i32)]));
                args.push(call(runtime.to_wasm_function_index(*fn_name), vec![]));
                args.push(call(runtime.drop_env, vec![]));
                Ok(seq(args))
            } else {
                Err(CompilationError::FunctionOutOfBounds {
                    fn_name_received: *fn_name,
                    number_of_all_functions_available_in_program: runtime.number_of_all_functions_available_in_program()
                })
            }
        },
        gmm::Term::PartialApply(fn_name, terms) => {
            todo!()
        },
        gmm::Term::CallClosure(closure_term, terms) => {
            todo!()
        },
        gmm::Term::VarUse(var_name) => {
            if *var_name < number_of_parameters {
                Ok(call(runtime.var, vec![i32_const(*var_name as i32)]))
            } else {
                Err(CompilationError::VariableOutOfBounds { var_index_received: *var_name, number_of_parameters })
            }
        },
        gmm::Term::Let(terms, body_term) => {
            let mut code = vec![];
            let mut number_of_parameters = number_of_parameters;
            code.push(call(runtime.copy_and_extend_env, vec![i32_const(0)]));
            for term in terms {
                code.push(compile_term(runtime, number_of_parameters, term)?);
                number_of_parameters += 1;
                code.push(call(runtime.extend_env, vec![]));
            }
            code.push(compile_term(runtime, number_of_parameters, body_term)?);
            code.push(call(runtime.drop_env, vec![]));
            Ok(seq(code))
        },
        gmm::Term::Match(arg, branches) => {
            // branches: Vec<(Pattern, Term)>
            todo!()
        },
        gmm::Term::Seq(terms) => {
            // TODO: I think I need to introduce am explicit pop instruction for the stack.
            todo!()
        },
    }
}

fn compile_function(runtime: &Runtime, number_of_parameters: usize, body: &gmm::Term) -> Result<TypedFunction> {
    Ok(TypedFunction {
        type_: fn_type(vec![], vec![]),
        locals: vec![],
        body: compile_term(&runtime, number_of_parameters, body)?,
    })
}

pub fn compile(program: gmm::Program, primitives: PrimitiveFunctions) -> Result<Module> {
    let mut module = Module::empty();
    let runtime = import_runtime(&mut module, &program, primitives);

    for function in program.functions {
        module.add_typed_function(compile_function(&runtime, function.number_of_parameters, &function.body)?);
    }

    // TODO: You need to setup initial empty environment
    let main = module.add_typed_function(compile_function(&runtime, 0, &program.main)?);
    module.add_export(Export { name: "main".to_string(), export_description: ExportDescription::Function(main) });

    Ok(module)
}
