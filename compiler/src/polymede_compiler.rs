use ast::base as polymede;
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
    number_of_primitive_functions: usize,
    function_count: FunctionIndex,
    function_mapping: HashMap<FunctionName, FunctionIndex>,
    functions: Vec<gmm::Function>,
    constructor_mapping: HashMap<ConstructorName, ConstructorIndex>,

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

    fn extend_pattern(&mut self, pattern: &polymede::Pattern) {
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
    pub fn new(number_of_primitive_functions: usize) -> Self {
        Self {
            number_of_primitive_functions,

            function_count: 0,
            function_mapping: HashMap::new(),
            functions: vec![],
            constructor_mapping: HashMap::new(),
            env: Env::new(),
        }
    }

    fn get_constructor_index(&self, constructor_name: &ConstructorName) -> Option<ConstructorIndex> {
        self.constructor_mapping.get(constructor_name).copied()
    }

    fn get_function_index(&self, function_name: &FunctionName) -> Option<FunctionIndex> {
        match self.function_mapping.get(function_name) {
            Some(i) => Some(i + self.number_of_primitive_functions),
            None => None
        }
    }

    fn add_function_name(&mut self, function_name: FunctionName) {
        self.function_mapping.insert(function_name, self.function_count);
        self.function_count += 1;
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

    fn extend_pattern(&mut self, pattern: &polymede::Pattern) {
        self.env.extend_pattern(pattern)
    }

    fn extend_vars(&mut self, vars: Vec<Variable>) {
        self.env.extend_vars(vars)
    }
    
    fn get_var(&self, var: &Variable) -> Option<&VariableIndex> {
        self.env.get(var)
    }
}

pub fn compile(number_of_primitive_functions: usize, program: &polymede::Program) -> gmm::Program {
    let mut state = State::new(number_of_primitive_functions);
    // ===constructors===
    for decl in program.type_declarations_in_source_ordering() {
        let mut count: ConstructorIndex = 0;

        for constructor in decl.constructors_in_source_ordering() {
            state.constructor_mapping.insert(constructor.name.clone(), count);
            count += 1;
        }
    }

    // ===functions===
    for decl in program.function_declarations_in_source_ordering() { 
        state.add_function_name(decl.name())
    }

    // TODO: This vector shuffling is way too complicated.
    let mut functions: Vec<(FunctionIndex, gmm::Function)> = vec![];
    for decl in program.function_declarations_in_source_ordering() { 
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
    state.open_env();
    state.extend_vars(function.parameters.clone());
    let gmm_function = gmm::Function {
        number_of_parameters: function.parameters.len(),
        body: compile_term(state, &function.body),
    };
    state.close_env();
    gmm_function
}

fn compile_term(state: &mut State, term: &polymede::Term) -> gmm::Term {
    use polymede::Term::*;
    match term {
        TypedTerm(typed_term) => compile_term(state, &typed_term.term),
        VariableUse(var) => {
            let Some(var_index) = state.get_var(var) else { unreachable!() };
            var_index.compile()
        },
        FunctionApplication(function_name, _, args) => {
            let Some(fn_index) = state.get_function_index(function_name) else { unreachable!() };
            gmm::Term::Call(fn_index, args.iter().map(|arg| compile_term(state, arg)).collect())
        },
        ConstructorUse(constructor_name, args) => {
            let Some(constructor_index) = state.get_constructor_index(constructor_name) else { unreachable!() };
            gmm::Term::Tuple(constructor_index, args.iter().map(|arg| compile_term(state, arg)).collect())
        },
        Match(arg, branches) => {
            // TODO: This will have to be replaced by proper
            //       compilation of nested patterns.
            let gmm_arg = compile_term(state, arg);
            let mut gmm_branches = vec![];
            for branch in branches {
                state.open_env();
                state.extend_pattern(&branch.pattern);
                use polymede::Pattern::*;
                let gmm_pattern = match &branch.pattern {
                    Constructor(constructor_name, _) => {
                        let Some(constructor_index) = state.get_constructor_index(constructor_name) else { unreachable!() };
                        gmm::Pattern::Variant(constructor_index)
                    },
                    Variable(_) => gmm::Pattern::Always,
                    Anything(_) => gmm::Pattern::Always,
                };
                gmm_branches.push((gmm_pattern, compile_term(state, &branch.body)));
                state.close_env();
            }
            gmm::Term::Match(Box::new(gmm_arg), gmm_branches)
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

fn pattern_to_indices(pattern: &polymede::Pattern, indices: &mut Vec<(Variable, VariableIndex)>, depth: Vec<u8>, var_index: usize) {
    use polymede::Pattern::*;
    match pattern {
        Constructor(_, patterns) => {
            for (i, pattern) in patterns.iter().enumerate() {
                let mut depth = depth.clone();
                depth.push(i as u8);
                pattern_to_indices(pattern, indices, depth, var_index)
            }
        },
        Variable(var) => {
            indices.push((var.clone(), VariableIndex { index: var_index, projections: depth }))
        },
        Anything(_) => {}
    }
}

// TODO: This function is used instead of `pattern_to_indices` so that we crash on nested patterns.
//       Nested patterns are not yet supported.
fn pattern_to_indices_simple(pattern: &polymede::Pattern, indices: &mut Vec<(Variable, VariableIndex)>, var_index: usize) {
    use polymede::Pattern::*;
    match pattern {
        Constructor(_, patterns) => {
            for (i, pattern) in patterns.iter().enumerate() {
                match pattern {
                    Constructor(_, _) => todo!("We don't yet support nested patterns during compilation."),
                    Variable(var) => {
                        indices.push((var.clone(), VariableIndex { index: var_index, projections: vec![i as u8] }))
                    }
                    Anything(_) => {}
                }
            }
        },
        Variable(var) => {
            indices.push((var.clone(), VariableIndex { index: var_index, projections: vec![] }))
        },
        Anything(_) => {}
    }
}
