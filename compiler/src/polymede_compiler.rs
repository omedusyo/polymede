use ast::base as polymede;
use ast::identifier::{FunctionName, ConstructorName, Variable};
use crate::graph_memory_machine as gmm;

use std::collections::HashMap;

type FunctionIndex = usize;
type VariableIndex = usize;
type ConstructorIndex = i32;

struct State {
    number_of_primitive_functions: usize,

    function_count: FunctionIndex,
    function_mapping: HashMap<FunctionName, FunctionIndex>,
    functions: Vec<gmm::Function>,
    constructor_mapping: HashMap<ConstructorName, ConstructorIndex>,
    env: Vec<Scope>,
}

struct Scope {
    count: VariableIndex,
    mapping: HashMap<Variable, VariableIndex>,
}

impl State {
    pub fn new(number_of_primitive_functions: usize) -> Self {
        Self {
            number_of_primitive_functions,

            function_count: 0,
            function_mapping: HashMap::new(),
            functions: vec![],
            constructor_mapping: HashMap::new(),
            env: vec![Scope::new()],
        }
    }

    fn get_constructor_index(&self, constructor_name: &ConstructorName) -> Option<ConstructorIndex> {
        self.constructor_mapping.get(constructor_name).copied()
    }

    fn get_function_index(&self, function_name: &FunctionName) -> Option<FunctionIndex> {
        self.function_mapping.get(function_name).copied()
    }

    fn add_function_name(&mut self, function_name: FunctionName) {
        self.function_mapping.insert(function_name, self.function_count);
        self.function_count += 1;
    }

    fn open_env(&mut self) {
        self.env.push(Scope::new())
    }

    fn close_env(&mut self) {
        let _ = self.env.pop();
    }

    fn extend_var(&mut self, var: Variable) {
        match self.env.last_mut() {
            Some(scope) => {
                scope.extend_var(var)
            },
            None => unreachable!(),
        }
    }

    fn extend_vars(&mut self, vars: Vec<Variable>) {
        match self.env.last_mut() {
            Some(scope) => {
                scope.extend_vars(vars)
            },
            None => unreachable!(),
        }
    }
    
    fn get_var(&self, var: &Variable) -> Option<VariableIndex> {
        for scope in self.env.iter().rev() {
            match scope.get(var) {
                Some(var_index) => return Some(var_index),
                None => {},
            }
        }
        None
    }
}

impl Scope {
    fn new() -> Self {
        Self { count: 0, mapping: HashMap::new() }
    }

    fn extend_var(&mut self, var: Variable) {
        self.mapping.insert(var, self.count);
        self.count += 1;
    }

    fn extend_vars(&mut self, vars: Vec<Variable>) {
        for var in vars {
            self.extend_var(var)
        }
    }

    fn get(&self, var: &Variable) -> Option<VariableIndex> {
        self.mapping.get(var).copied()
    }
}

pub fn compile(number_of_primitive_functions: usize, program: &polymede::Program) -> gmm::Program {
    let mut state = State::new(number_of_primitive_functions);
    // ===constructors===
    for decl in program.type_declarations.values() {
        let mut count: ConstructorIndex = 0;

        for constructor in decl.constructors().values() {
            state.constructor_mapping.insert(constructor.name.clone(), count);
            count += 1;
        }
    }

    // ===functions===
    for decl in program.function_declarations.values() { 
        state.add_function_name(decl.name())
    }

    // TODO: This vector shuffling is way too complicated.
    let mut functions: Vec<(FunctionIndex, gmm::Function)> = vec![];
    for decl in program.function_declarations.values() { 
        let Some(fn_index) = state.get_function_index(&decl.name()) else { unreachable!() };
        functions.push((fn_index, compile_function_declaration(&mut state, &decl.function.function)));
    }
    functions.sort_by(|(i, _), (j, _)| i.partial_cmp(j).unwrap());

    for (_, function) in functions {
        state.functions.push(function)
    }

    gmm::Program {
        number_of_primitive_functions: state.number_of_primitive_functions,
        functions: state.functions,
        main: gmm::constant(0), // TODO
    }
}

fn compile_function_declaration(state: &mut State, function: &polymede::Function) -> gmm::Function {
    state.extend_vars(function.parameters.clone());
    gmm::Function {
        number_of_parameters: function.parameters.len(),
        body: compile_term(state, &function.body),
    }
}

fn compile_term(state: &mut State, term: &polymede::Term) -> gmm::Term {
    use polymede::Term::*;
    match term {
        TypedTerm(typed_term) => compile_term(state, &typed_term.term),
        VariableUse(var) => {
            let Some(var_index) = state.get_var(var) else { unreachable!() };
            gmm::Term::VarUse(var_index)
        },
        FunctionApplication(function_name, _, args) => {
            let Some(fn_index) = state.get_function_index(function_name) else { unreachable!() };
            gmm::Term::Call(fn_index, args.iter().map(|arg| compile_term(state, arg)).collect())
        },
        ConstructorUse(constructor_name, args) => {
            let Some(constructor_index) = state.get_constructor_index(constructor_name) else { unreachable!() };
            gmm::Term::Tuple(constructor_index, args.iter().map(|arg| compile_term(state, arg)).collect())
        },
        Match(arg, pattern_branch) => {
            todo!()
        },
        Fold(arg, pattern_branch) => {
            todo!()
        },
        Lambda(function) => {
            todo!()
        },
        LambdaApplication(fn_term, args) => {
            todo!()
        },
        Let(bindings, body) => {
            state.open_env();
            let mut vars: Vec<Variable> = Vec::with_capacity(bindings.len());
            let mut gmm_args: Vec<gmm::Term> = Vec::with_capacity(bindings.len());
            for (var, term) in bindings {
                vars.push(var.clone());
                state.extend_var(var.clone());
                gmm_args.push(compile_term(state, term))
            }
            let gmm_body = compile_term(state, body);
            state.close_env();
            gmm::Term::Let(gmm_args, Box::new(gmm_body))
        },
    }
}
