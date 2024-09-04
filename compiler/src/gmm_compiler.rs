use crate::graph_memory_machine as gmm;
use crate::runtime::Runtime;
use std::collections::HashMap;
use wasm::{
    base::{
        export::{Export, ExportDescription},
        indices::{FunctionIndex, TableIndex, TypeIndex},
        types::{BlockType, FunctionType, GlobalType, Mutability, NumType, ValueType},
    },
    syntax::{
        call, call_indirect, f32_const, fn_type, i32_const, i32_eq, return_call, seq,
        CustomSection, Expression, Global, Module, TypedFunction, TypedFunctionImport,
    },
};

#[derive(Debug)]
pub enum CompilationError {
    VariableOutOfBounds {
        var_index_received: gmm::VarName,
        number_of_parameters: usize,
    },
    FunctionOutOfBounds {
        fn_name_received: gmm::FunctionName,
        number_of_all_functions_available_in_program: usize,
    },
}

pub type Result<A> = std::result::Result<A, CompilationError>;

type FunctionTableIndex = i32;

type RawPointerToStatic = i32;

#[derive(Debug)]
struct State {
    gmm_to_wasm_name_map: HashMap<gmm::FunctionName, FunctionInfo>,

    runtime: Runtime,

    function_type_for_call_indirect: TypeIndex,
    function_table_map: HashMap<FunctionIndex, FunctionTableIndex>,
    function_table: Vec<FunctionIndex>,

    current_static_offset: RawPointerToStatic,
    byte_array_store: Vec<(RawPointerToStatic, Vec<u8>)>,
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

impl State {
    fn new(module: &mut Module, runtime: Runtime) -> Self {
        let function_type_for_call_indirect = module.add_function_type(FunctionType {
            domain: vec![],
            codomain: vec![],
        });
        Self {
            gmm_to_wasm_name_map: HashMap::new(),

            runtime,

            function_type_for_call_indirect,
            function_table_map: HashMap::new(),
            function_table: vec![],

            current_static_offset: 0,
            byte_array_store: vec![],
        }
    }

    fn function_info(&self, fn_name: gmm::FunctionName) -> Result<FunctionInfo> {
        match self.gmm_to_wasm_name_map.get(&fn_name) {
            Some(fn_info) => Ok(*fn_info),
            None => Err(CompilationError::FunctionOutOfBounds {
                fn_name_received: fn_name,
                number_of_all_functions_available_in_program: self
                    .runtime
                    .number_of_runtime_functions
                    + self.gmm_to_wasm_name_map.len(),
            }),
        }
    }

    fn wasm_function_index(&self, fn_name: gmm::FunctionName) -> Result<FunctionIndex> {
        let fn_info = self.function_info(fn_name)?;
        Ok(fn_info.function_index)
    }

    fn register_function_in_function_table(
        &mut self,
        fn_name: gmm::FunctionName,
    ) -> Result<FunctionTableIndex> {
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

    fn register_byte_array_literal(&mut self, bytes: Vec<u8>) -> RawPointerToStatic {
        let raw_pointer: RawPointerToStatic = self.current_static_offset;
        self.current_static_offset += Runtime::BYTE_ARRAY_HEADER_BYTE_SIZE + (bytes.len() as i32);
        self.byte_array_store.push((raw_pointer, bytes));
        raw_pointer
    }
}

// ===Imports===
pub fn import_runtime_function(
    module: &mut Module,
    fn_name: &str,
    type_: FunctionType,
) -> FunctionIndex {
    module.add_typed_function_import(TypedFunctionImport {
        module_name: "runtime".to_string(),
        name: fn_name.to_string(),
        type_,
    })
}

fn import_primitive_function(module: &mut Module, external_name: String) -> FunctionIndex {
    let type_ = FunctionType {
        domain: vec![],
        codomain: vec![],
    };
    module.add_typed_function_import(TypedFunctionImport {
        module_name: "primitives".to_string(),
        name: external_name,
        type_,
    })
}
// ===Compilation===
pub fn compile(program: gmm::Program) -> Result<Module> {
    let mut module = Module::empty();
    let runtime = Runtime::import(&mut module);
    let mut state = State::new(&mut module, runtime);

    // ===Split program.functions into two separate vectors of primitive imports and user-defined functions===
    let mut gmm_imports: Vec<(gmm::FunctionName, gmm::FunctionImport)> = vec![];
    let mut gmm_functions: Vec<(gmm::FunctionName, gmm::Function)> = vec![];
    for (next_gmm_function_name, function) in program.functions.into_iter().enumerate() {
        match function {
            gmm::FunctionOrImport::Fn(function) => {
                gmm_functions.push((next_gmm_function_name, function))
            }
            gmm::FunctionOrImport::Import(import) => {
                gmm_imports.push((next_gmm_function_name, import))
            }
        }
    }

    // ===Setup `state.gmm_to_wasm_name_map`===
    {
        let mut next_wasm_function_index = state.runtime.number_of_runtime_functions;
        for (gmm_function_name, _) in &gmm_imports {
            state.gmm_to_wasm_name_map.insert(
                *gmm_function_name,
                FunctionInfo {
                    function_index: FunctionIndex(next_wasm_function_index as u32),
                    kind: FunctionKind::Primitive,
                },
            );
            next_wasm_function_index += 1;
        }

        for (gmm_function_name, _) in &gmm_functions {
            state.gmm_to_wasm_name_map.insert(
                *gmm_function_name,
                FunctionInfo {
                    function_index: FunctionIndex(next_wasm_function_index as u32),
                    kind: FunctionKind::UserDefined,
                },
            );
            next_wasm_function_index += 1;
        }
    }

    // Compile imports.
    for (_, gmm_import) in gmm_imports {
        import_primitive_function(&mut module, gmm_import.external_name);
    }
    // Compile user-defined functions.
    for (_, function) in gmm_functions {
        module.add_typed_function(compile_function(
            &mut state,
            function.number_of_parameters,
            &function.body,
        )?);
    }

    // Compile main.
    let main = module.add_typed_function(compile_main(&mut state, &program.main)?);
    module.add_export(Export {
        name: "main".to_string(),
        export_description: ExportDescription::Function(main),
    });
    module.add_export(Export {
        name: "function_table".to_string(),
        export_description: ExportDescription::Table(TableIndex(0)),
    });

    // TODO: This doesnt' work when wasm-merge is used. It is merged in such a way that I have
    // multiple memory imports. Jesus... I can resolve this by having the string literals compiled
    // into the code with passive data segments. Afterwards initializing the STATIC manually.
    //module.add_memory_import(MemoryImport { module_name: "runtime".to_string(), name: "memory".to_string(), limit: Limit::MinToInfinity { min: 0 } });

    module.register_function_table(state.function_table);

    {
        let mut polymede_static = vec![];
        for (_, bytes) in state.byte_array_store {
            polymede_static.extend(Runtime::encode_byte_array(&bytes));
        }

        // Global variable that stores the size of the STATIC region in bytes.
        let static_size_global_index = module.add_global(Global {
            global_type: GlobalType {
                type_: ValueType::NumType(NumType::I32),
                mutability: Mutability::Const,
            },
            expression: i32_const(polymede_static.len() as i32),
        });
        module.add_custom_section_at_the_end(CustomSection {
            name: "POLYMEDE_STATIC".to_string(),
            bytes: polymede_static,
        });
        // Note that I don't really need to export the global, since this information is contained
        // in the size of the custom section.
        module.add_export(Export {
            name: "STATIC_SIZE".to_string(),
            export_description: ExportDescription::Global(static_size_global_index),
        });
    }

    Ok(module)
}

fn compile_main(state: &mut State, body: &gmm::Term) -> Result<TypedFunction> {
    Ok(TypedFunction {
        type_: fn_type(vec![], vec![]),
        locals: vec![],
        body: {
            seq(vec![
                call(state.runtime.make_env, vec![i32_const(0)]),
                compile_term(state, 0, body, true)?,
            ])
        },
    })
}

fn compile_function(
    state: &mut State,
    number_of_parameters: usize,
    body: &gmm::Term,
) -> Result<TypedFunction> {
    Ok(TypedFunction {
        type_: fn_type(vec![], vec![]),
        locals: vec![],
        body: compile_term(state, number_of_parameters, body, true)?,
    })
}

fn compile_terms(
    state: &mut State,
    number_of_parameters: usize,
    terms: &[gmm::Term],
) -> Result<Vec<Expression>> {
    let mut args: Vec<Expression> = Vec::with_capacity(terms.len());
    for term in terms {
        let expression = compile_term(state, number_of_parameters, term, false)?;
        args.push(expression);
    }
    Ok(args)
}

fn compile_term(
    state: &mut State,
    number_of_parameters: usize,
    term: &gmm::Term,
    tail_position: bool,
) -> Result<Expression> {
    match term {
        gmm::Term::Const(variant) => {
            let mut code = vec![];
            code.push(call(state.runtime.const_, vec![i32_const(*variant)]));

            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::Float32(x) => {
            let mut code = vec![];
            code.push(call(state.runtime.float32, vec![f32_const(*x)]));

            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::ByteArray(bytes) => {
            let mut code = vec![];
            let byte_count = bytes.len() as i32;
            // TODO: Again I'm cloning the byte array literal.
            let pointer = state.register_byte_array_literal(bytes.to_vec());
            code.push(call(
                state.runtime.array_slice,
                vec![
                    i32_const(pointer),
                    i32_const(pointer + Runtime::BYTE_ARRAY_HEADER_BYTE_SIZE),
                    i32_const(byte_count),
                ],
            ));

            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::Tuple(variant, terms) => {
            let mut code: Vec<Expression> = compile_terms(state, number_of_parameters, terms)?;
            code.push(call(
                state.runtime.tuple,
                vec![i32_const(*variant), i32_const(terms.len() as i32)],
            ));

            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::ProjectComponent(term, component_index) => {
            let mut code = vec![compile_term(state, number_of_parameters, term, false)?];
            code.push(call(
                state.runtime.tuple_project,
                vec![i32_const(*component_index as i32)],
            ));

            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::Call(fn_name, terms) => {
            let FunctionInfo {
                function_index,
                kind,
            } = state.function_info(*fn_name)?;
            let mut code: Vec<Expression> = compile_terms(state, number_of_parameters, terms)?;
            match kind {
                FunctionKind::Primitive => {
                    code.push(call(function_index, vec![]));
                    if tail_position {
                        code.push(call(state.runtime.drop_env, vec![]))
                    }
                }
                FunctionKind::UserDefined => {
                    if tail_position {
                        code.push(call(state.runtime.drop_env_then_shift, vec![]));
                        code.push(call(
                            state.runtime.make_env,
                            vec![i32_const(terms.len() as i32)],
                        ));
                        code.push(return_call(function_index, vec![]));
                    } else {
                        code.push(call(
                            state.runtime.make_env,
                            vec![i32_const(terms.len() as i32)],
                        ));
                        code.push(call(function_index, vec![]));
                    }
                }
            };
            Ok(seq(code))
        }
        gmm::Term::PartialApply(fn_name, terms) => {
            let mut code: Vec<Expression> = compile_terms(state, number_of_parameters, terms)?;
            let fn_table_index: FunctionTableIndex =
                state.register_function_in_function_table(*fn_name)?;
            code.push(call(
                state.runtime.partial_apply,
                vec![i32_const(fn_table_index), i32_const(terms.len() as i32)],
            ));

            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::CallClosure(closure_term, terms) => {
            let mut code: Vec<Expression> = vec![compile_term(
                state,
                number_of_parameters,
                closure_term,
                false,
            )?];
            for compiled_term in compile_terms(state, number_of_parameters, terms)? {
                code.push(compiled_term)
            }
            if tail_position {
                code.push(call(state.runtime.drop_env_then_shift, vec![]));
                code.push(call_indirect(
                    state.function_type_for_call_indirect,
                    TableIndex(0),
                    vec![call(
                        state.runtime.make_env_from_closure,
                        vec![i32_const(terms.len() as i32)],
                    )],
                ));
            } else {
                code.push(call_indirect(
                    state.function_type_for_call_indirect,
                    TableIndex(0),
                    vec![call(
                        state.runtime.make_env_from_closure,
                        vec![i32_const(terms.len() as i32)],
                    )],
                ));
            }
            Ok(seq(code))
        }
        gmm::Term::VarUse(var_name) => {
            if *var_name < number_of_parameters {
                let mut code: Vec<Expression> =
                    vec![call(state.runtime.var, vec![i32_const(*var_name as i32)])];
                if tail_position {
                    code.push(call(state.runtime.drop_env, vec![]))
                }
                Ok(seq(code))
            } else {
                Err(CompilationError::VariableOutOfBounds {
                    var_index_received: *var_name,
                    number_of_parameters,
                })
            }
        }
        gmm::Term::Let(terms, body_term) => {
            let mut code = vec![];
            let mut number_of_parameters = number_of_parameters;
            if tail_position {
                // Note we're not calling copy_and_extend_env - we're just extending current
                // environment which is currently on top of the stack.
                for term in terms {
                    code.push(compile_term(state, number_of_parameters, term, false)?);
                    number_of_parameters += 1;
                    code.push(call(state.runtime.extend_env, vec![i32_const(1)]));
                }
                // This is responsible for eventually generating drop_env, which will close the
                // current function's/let's env.
                code.push(compile_term(state, number_of_parameters, body_term, true)?);
            } else {
                code.push(call(state.runtime.copy_and_extend_env, vec![i32_const(0)]));
                for term in terms {
                    code.push(compile_term(state, number_of_parameters, term, false)?);
                    number_of_parameters += 1;
                    code.push(call(state.runtime.extend_env, vec![i32_const(1)]));
                }
                code.push(compile_term(state, number_of_parameters, body_term, false)?);
                code.push(call(state.runtime.drop_env, vec![])); // Closes the let's environment.
                                                                 // And since this is not a tail position, we do not close current function's env.
            }
            Ok(seq(code))
        }
        gmm::Term::Match(arg_term, branches) => {
            let mut code = vec![];
            code.push(compile_term(state, number_of_parameters, arg_term, false)?);
            if tail_position {
                code.push(call(state.runtime.extend_env, vec![i32_const(1)]));
                let arg_index = number_of_parameters;
                code.push(compile_branches(
                    state,
                    number_of_parameters + 1,
                    arg_index as i32,
                    branches,
                    true,
                )?);
            } else {
                code.push(call(state.runtime.copy_and_extend_env, vec![i32_const(1)]));
                let arg_index = number_of_parameters;
                // TODO: We extend the environment with the argument, which currently is extremely
                // inefficient.
                code.push(compile_branches(
                    state,
                    number_of_parameters + 1,
                    arg_index as i32,
                    branches,
                    false,
                )?);
                code.push(call(state.runtime.drop_env, vec![])); // closes the match's env.
            }
            Ok(seq(code))
        }
        gmm::Term::CommandAndThen(cmd_term, continuation_term) => {
            let mut code = vec![
                // WARNING: When changing this, take a look at perform_command.js
                compile_term(state, number_of_parameters, &continuation_term.body, false)?,
                compile_term(state, number_of_parameters, cmd_term, false)?,
                call(state.runtime.and_then, vec![]),
            ];
            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::Pure(term) => {
            let mut code = vec![];
            code.push(compile_term(state, number_of_parameters, term, false)?);
            code.push(call(state.runtime.pure, vec![]));
            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
        gmm::Term::Receive => {
            let mut code = vec![];
            code.push(call(state.runtime.receive, vec![]));
            if tail_position {
                code.push(call(state.runtime.drop_env, vec![]))
            }
            Ok(seq(code))
        }
    }
}

fn compile_branches(
    state: &mut State,
    number_of_parameters: usize,
    arg_index: i32,
    branches: &[(gmm::Pattern, gmm::Term)],
    tail_position: bool,
) -> Result<Expression> {
    if branches.is_empty() {
        let mut code = vec![];
        code.push(Expression::Unreachable);

        if tail_position {
            code.push(call(state.runtime.drop_env, vec![]))
        }
        Ok(seq(code))
    } else {
        let (pattern, body) = &branches[0];
        match pattern {
            gmm::Pattern::Variant(variant) => Ok(Expression::IfThenElse {
                type_: BlockType::EmptyType,
                test: Box::new(seq(vec![
                    call(state.runtime.var, vec![i32_const(arg_index)]),
                    i32_eq(call(state.runtime.get_variant, vec![]), i32_const(*variant)),
                ])),
                then_body: Box::new(compile_term(
                    state,
                    number_of_parameters,
                    body,
                    tail_position,
                )?),
                else_body: Box::new(compile_branches(
                    state,
                    number_of_parameters,
                    arg_index,
                    &branches[1..],
                    tail_position,
                )?),
            }),
            gmm::Pattern::Always => compile_term(state, number_of_parameters, body, tail_position),
        }
    }
}
