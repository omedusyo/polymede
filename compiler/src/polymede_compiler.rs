use ast::base as polymede;
use ast::desugared_base as desugared_polymede;
use ast::identifier::{FunctionName, ConstructorName, Variable};
use crate::graph_memory_machine as gmm;

use std::collections::HashMap;

type FunctionIndex = usize;
type ConstructorIndex = i32;

struct VariableIndex {
    index: usize,
    projections: Vec<u8>,
}

struct State {
    constructor_mapping: HashMap<ConstructorName, ConstructorIndex>,

    function_mapping: HashMap<FunctionName, FunctionIndex>,
    anonymous_functions: Vec<gmm::Function>,

    env: Env,
}

struct Env {
    scopes: Vec<Scope>,
}

struct Scope {
    next_var: usize,
    map: HashMap<Variable, VariableIndex>,
}

impl VariableIndex {
    fn compile(&self) -> gmm::Term {
        // [i0, i1, i2] ~> ProjectComponent(ProjectComponent(ProjectComponent(VarUse(index), i2), i1), i0)
        let mut term = gmm::Term::VarUse(self.index);
        for i in self.projections.iter().rev() {
            term = gmm::Term::ProjectComponent(Box::new(term), *i)
        }
        term
    }
}


impl Env {
    fn new() -> Self {
        Self { scopes: vec![] }
    }

    fn open_env(&mut self) { 
        match self.scopes.last() {
            Some(top_scope) => {
                self.scopes.push(Scope { next_var: top_scope.next_var, map: HashMap::new() })
            },
            None => {
                self.scopes.push(Scope { next_var: 0, map: HashMap::new() })
            }
        }
    }

    fn close_env(&mut self) {
        let _ = self.scopes.pop();
    }

    fn extend_var(&mut self, var: Variable) {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.map.insert(var, VariableIndex { index: scope.next_var, projections: vec![] });
                scope.next_var += 1;
            },
            None => unreachable!(),
        }
    }

    fn extend_pattern(&mut self, pattern: &desugared_polymede::Pattern) {
        match self.scopes.last_mut() {
            Some(scope) => {
                let mut indices = vec![];
                pattern_to_indices_simple(pattern, &mut indices, scope.next_var);
                for (var, index) in indices {
                    scope.map.insert(var, index);
                }
                scope.next_var += 1;
            },
            None => unreachable!(),
        }
    }

    fn extend_vars(&mut self, vars: Vec<Variable>) {
        for var in vars {
            self.extend_var(var)
        }
    }

    fn get(&self, var: &Variable) -> Option<&VariableIndex> {
        for scope in self.scopes.iter().rev() {
            match scope.map.get(var) {
                Some(var_index) => return Some(var_index),
                None => {},
            }
        }
        None
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            constructor_mapping: HashMap::new(),
            function_mapping: HashMap::new(),
            anonymous_functions: vec![],
            env: Env::new(),
        }
    }

    fn reset_env(&mut self) -> Env {
        use std::mem;
        let old_env = mem::replace(&mut self.env, Env::new());
        old_env
    }

    fn restore_env(&mut self, env: Env) {
        self.env = env;
    }

    fn get_constructor_index(&self, constructor_name: &ConstructorName) -> Option<ConstructorIndex> {
        self.constructor_mapping.get(constructor_name).copied()
    }

    fn get_function_index(&self, function_name: &FunctionName) -> Option<FunctionIndex> {
        self.function_mapping.get(function_name).copied()
    }

    fn add_anonymous_function(&mut self, gmm_function: gmm::Function) -> usize {
        let anonymous_fn_index = self.anonymous_functions.len();
        self.anonymous_functions.push(gmm_function);

        self.function_mapping.len() + anonymous_fn_index
    }

    fn open_env(&mut self) {
        self.env.open_env()
    }

    fn close_env(&mut self) {
        self.env.close_env()
    }

    fn extend_var(&mut self, var: Variable) {
        self.env.extend_var(var)
    }

    fn extend_pattern(&mut self, pattern: &desugared_polymede::Pattern) {
        self.env.extend_pattern(pattern)
    }

    fn extend_vars(&mut self, vars: Vec<Variable>) {
        self.env.extend_vars(vars)
    }
    
    fn get_var(&self, var: &Variable) -> Option<&VariableIndex> {
        self.env.get(var)
    }
}

pub fn compile(program: &polymede::Program) -> gmm::Program {
    let mut state = State::new();
    // ===Constructor Name ~> Constructor Index===
    for decl in program.type_declarations_in_source_ordering() {
        let mut count: ConstructorIndex = 0;

        for constructor in decl.constructors_in_source_ordering() {
            state.constructor_mapping.insert(constructor.name.clone(), count);
            count += 1;
        }
    }

    // ===Function Name ~> Function Index===
    {
        let mut next_function_index: FunctionIndex = 0;
        for decl in program.function_declarations_in_source_ordering() { 
            state.function_mapping.insert(decl.name(), next_function_index);
            next_function_index += 1;
        }
    }
    
    // ===Main===
    let Some(run) = &program.run_declaration else { unreachable!() };
    let main = compile_typed_term(&mut state, &desugared_polymede::desugar_typed_term(&run.body));


    // ===Functions===
    let mut functions: Vec<gmm::FunctionOrImport> = Vec::with_capacity(state.function_mapping.len());
    for decl in program.function_declarations_in_source_ordering() { 
        match decl {
            polymede::FunctionDeclaration::User(decl) => {
                functions.push(gmm::FunctionOrImport::Fn(compile_function(&mut state, &desugared_polymede::desugar_function(&decl.function.function))));
            },
            polymede::FunctionDeclaration::Foreign(decl) => {
                let gmm_import = gmm::FunctionImport {
                    number_of_parameters: decl.type_.input_types.len(),
                    external_name: decl.external_name.str(program.interner()).to_string(),
                };
                functions.push(gmm::FunctionOrImport::Import(gmm_import));
            }
        }
    }

    for anon_fn in state.anonymous_functions {
        functions.push(gmm::FunctionOrImport::Fn(anon_fn));
    }

    gmm::Program {
        functions,
        main,
    }
}

fn compile_function(state: &mut State, function: &desugared_polymede::Function) -> gmm::Function {
    state.open_env();
    state.extend_vars(function.parameters.clone());
    let gmm_function = gmm::Function {
        number_of_parameters: function.parameters.len(),
        body: compile_term(state, &function.body),
    };
    state.close_env();
    gmm_function
}

fn compile_var(state: &State, var: &Variable) -> gmm::Term {
    let Some(var_index) = state.get_var(var) else { unreachable!() };
    var_index.compile()
}

fn compile_typed_term(state: &mut State, typed_term: &desugared_polymede::TypedTerm) -> gmm::Term {
    compile_term(state, &typed_term.term)
}

// TODO: Consider introducing two versions for better performance
//   fn compile_term(state: &mut State, term: &desugared_polymede::Term) -> gmm::Term
//   fn compile_term(state: &mut State, term: desugared_polymede::Term) -> gmm::Term
fn compile_term(state: &mut State, term: &desugared_polymede::Term) -> gmm::Term {
    use desugared_polymede::Term::*;
    match term {
        TypedTerm(typed_term) => compile_typed_term(state, &typed_term),
        VariableUse(var) => compile_var(state, var),
        Int(x) => gmm::Term::Const(*x) ,
        Float(x) => gmm::Term::Float32(*x),
        StringLiteral(s) => gmm::Term::ByteArray(s.clone().into_bytes()), // TODO: Again, this clones the string, which really sucks.
        FunctionApplication(function_name, _, args) => {
            let Some(fn_index) = state.get_function_index(function_name) else { unreachable!() };
            gmm::Term::Call(fn_index, args.iter().map(|arg| compile_term(state, arg)).collect())
        },
        ConstructorUse(constructor_name, args) => {
            let Some(constructor_index) = state.get_constructor_index(constructor_name) else { unreachable!() };
            if args.is_empty() {
                gmm::Term::Const(constructor_index)
            } else {
                gmm::Term::Tuple(constructor_index, args.iter().map(|arg| compile_term(state, arg)).collect())
            }
        },
        Match(arg, branches) => {
            let gmm_arg = compile_term(state, arg);
            let mut gmm_branches = vec![];
            for branch in branches {
                state.open_env();
                state.extend_pattern(&branch.pattern);
                use desugared_polymede::Pattern::*;
                let gmm_pattern = match &branch.pattern {
                    Constructor(constructor_name, _) => {
                        let Some(constructor_index) = state.get_constructor_index(constructor_name) else { unreachable!() };
                        gmm::Pattern::Variant(constructor_index)
                    },
                    Int(x) => gmm::Pattern::Variant(*x),
                    Variable(_) => gmm::Pattern::Always,
                    Anything(_) => gmm::Pattern::Always,
                };
                gmm_branches.push((gmm_pattern, compile_term(state, &branch.body)));
                state.close_env();
            }
            gmm::Term::Match(Box::new(gmm_arg), gmm_branches)
        },
        Lambda(function) => {
            compile_closure(state, function)
        },
        LambdaApplication(fn_term, args) => {
            let gmm_fn_term = compile_term(state, fn_term);
            let gmm_args = args.iter().map(|arg| compile_term(state, arg)).collect();
            gmm::Term::CallClosure(Box::new(gmm_fn_term), gmm_args)
        },
        Let(bindings, body) => {
            state.open_env();
            let mut gmm_args: Vec<gmm::Term> = Vec::with_capacity(bindings.len());
            for (var, term) in bindings {
                gmm_args.push(compile_term(state, term));
                state.extend_var(var.clone());
            }
            let gmm_body = compile_term(state, body);
            state.close_env();
            gmm::Term::Let(gmm_args, Box::new(gmm_body))
        },
        Pure(term) => gmm::Term::Pure(Box::new(compile_term(state, term))),
        AndThen(cmd_term, continuation) => {
            gmm::Term::CommandAndThen(
                Box::new(compile_term(state, cmd_term)),
                Box::new(gmm::Continuation { body : compile_closure(state, continuation) })
            )
        },
    }
}

fn compile_closure(state: &mut State, function: &desugared_polymede::Function) -> gmm::Term {
    // 1. Compile anonymous lambda into a new global function with extended parameters.
    // 2. Afterwards partially apply said global function with values of free variables.
    let free_vars: Vec<Variable> = function.free_variables();
    let gmm_function = {
        let mut function_with_extended_parameters = function.clone();
        let mut new_parameters = free_vars.clone();
        new_parameters.append(&mut function_with_extended_parameters.parameters);
        function_with_extended_parameters.parameters = new_parameters;

        let old_env = state.reset_env();
        let gmm_function = compile_function(state, &function_with_extended_parameters);
        state.restore_env(old_env);
        gmm_function
    };
    let function_index = state.add_anonymous_function(gmm_function);

    let free_args = free_vars.iter().map(|var| compile_var(state, var)).collect();
    gmm::Term::PartialApply(function_index, free_args)
}

fn pattern_to_indices_simple(pattern: &desugared_polymede::Pattern, indices: &mut Vec<(Variable, VariableIndex)>, var_index: usize) {
    use desugared_polymede::Pattern::*;
    match pattern {
        Constructor(_, patterns) => {
            for (i, var) in patterns.iter().enumerate() {
                indices.push((var.clone(), VariableIndex { index: var_index, projections: vec![i as u8] }))
            }
        },
        Variable(var) => {
            indices.push((var.clone(), VariableIndex { index: var_index, projections: vec![] }))
        },
        Int(_) => {},
        Anything(_) => {}
    }
}
