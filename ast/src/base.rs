use crate::identifier::{Interner, interner, Variable, ConstructorName, FunctionName, ExternalName, Identifier};
use crate::parser::base::PreTypeDeclaration;
use std::collections::HashMap;
use std::collections::HashSet;

// ===Program===
#[derive(Debug)]
pub struct Program {
    interner: Interner,
    pub type_declarations: HashMap<Variable, TypeDeclaration>,
    pub type_declarations_ordering: Vec<Variable>,
    pub function_declarations: HashMap<FunctionName, FunctionDeclaration>,
    pub function_declarations_ordering: Vec<FunctionName>,
    pub run_declaration: Option<RunDeclaration>,
    pub constructor_to_type_mapping: HashMap<ConstructorName, Variable>,
}

impl Program {
    pub fn new() -> Self {
        Self { 
            interner: interner(),
            type_declarations: HashMap::new(),
            type_declarations_ordering: vec![],
            function_declarations: HashMap::new(),
            function_declarations_ordering: vec![],
            run_declaration: None,
            constructor_to_type_mapping: HashMap::new(),
        }
    }

    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    pub fn get_type_declaration(&self, type_name: &Variable) -> Option<&TypeDeclaration> {
        self.type_declarations.get(type_name)
    }

    pub fn get_function_declaration(&self, function_name: &FunctionName) -> Option<&FunctionDeclaration> {
        self.function_declarations.get(function_name)
    }

    pub fn get_type_declaration_of_constructor(&self, constructor_name: &ConstructorName) -> Option<&TypeDeclaration> {
        let type_name = self.constructor_to_type_mapping.get(constructor_name)?;
        self.get_type_declaration(type_name)
    }

    pub fn type_declarations_in_source_ordering(&self) -> Vec<&TypeDeclaration> {
        let mut result = vec![];
        for type_name in &self.type_declarations_ordering {
            result.push(self.get_type_declaration(type_name).unwrap())
        }
        result
    }
    
    pub fn function_declarations_in_source_ordering(&self) -> Vec<&FunctionDeclaration> {
        let mut result = vec![];
        for function_name in &self.function_declarations_ordering {
            result.push(self.get_function_declaration(function_name).unwrap())
        }
        result
    }
}


// ===Declarations===
#[derive(Debug)]
pub struct ConstructorDeclaration {
    pub name: Identifier,
    pub parameters: Vec<Type>,
}

#[derive(Debug)]
pub struct EnumDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub constructors: HashMap<ConstructorName, ConstructorDeclaration>,
    pub constructors_ordering: Vec<ConstructorName>,
}

#[derive(Debug)]
pub struct IndDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub recursive_type_var: Variable,
    pub constructors: HashMap<ConstructorName, ConstructorDeclaration>,
    pub constructors_ordering: Vec<ConstructorName>,
}

#[derive(Debug)]
pub enum TypeDeclaration {
    Enum(EnumDeclaration),
    Ind(IndDeclaration),
}

#[derive(Debug)]
pub enum FunctionDeclaration {
    User(UserFunctionDeclaration),
    Foreign(ForeignFunctionDeclaration),
}

#[derive(Debug)]
pub struct UserFunctionDeclaration {
    pub name: FunctionName,
    pub type_parameters: Vec<Variable>,
    pub function: TypedFunction,
}

#[derive(Debug)]
pub struct ForeignFunctionDeclaration {
    pub name: FunctionName,
    pub type_: FunctionType,
    pub external_name: ExternalName,
}

#[derive(Debug)]
pub struct TypedFunction {
    pub type_: FunctionType,
    pub function: Function,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Variable>,
    pub body: Term,
}

#[derive(Debug)]
pub struct RunDeclaration {
    pub body: TypedTerm,
}

// ===Types===
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    VariableUse(Variable),
    TypeApplication(ConstructorName, Vec<Type>),
    Arrow(Box<FunctionType>),
    I32,
    Command(Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub input_types: Vec<Type>,
    pub output_type: Type,
}


// ===Terms===
#[derive(Debug, Clone)]
pub struct TypedTerm {
    pub type_: Type,
    pub term: Term
}

#[derive(Debug, Clone)]
pub enum Term {
    TypedTerm(Box<TypedTerm>),
    Int(i32),
    VariableUse(Variable),
    FunctionApplication(Variable, Vec<Type>, Vec<Term>),
    ConstructorUse(ConstructorName, Vec<Term>),
    Match(Box<Term>, Vec<PatternBranch>),
    Fold(Box<Term>, Vec<PatternBranch>),
    Lambda(Box<Function>),
    LambdaApplication(Box<Term>, Vec<Term>),
    Let(Vec<(Variable, Term)>, Box<Term>),
    Pure(Box<Term>),
}

#[derive(Debug, Clone)]
pub struct PatternBranch {
    pub pattern: Pattern,
    pub body: Term,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Constructor(ConstructorName, Vec<Pattern>),
    Variable(Variable),
    Int(i32),
    Anything(Identifier),
}

impl TypeDeclaration {
    pub fn new(pre_decl: PreTypeDeclaration, constructor_to_type_mapping: &mut HashMap<ConstructorName, Variable>) -> Self {
        match pre_decl {
            PreTypeDeclaration::Enum(pre_decl) => {
                let mut constructors = HashMap::new();
                let mut constructors_ordering = vec![];
                for constructor_decl in pre_decl.constructors {
                    constructor_to_type_mapping.insert(constructor_decl.name.clone(), pre_decl.name.clone());
                    constructors_ordering.push(constructor_decl.name.clone()); 
                    constructors.insert(constructor_decl.name.clone(), constructor_decl);
                }
                Self::Enum(EnumDeclaration {
                    name: pre_decl.name,
                    type_parameters: pre_decl.type_parameters,
                    constructors,
                    constructors_ordering,
                })
            },
            PreTypeDeclaration::Ind(pre_decl) => {
                let mut constructors = HashMap::new();
                let mut constructors_ordering = vec![];
                for constructor_decl in pre_decl.constructors {
                    constructor_to_type_mapping.insert(constructor_decl.name.clone(), pre_decl.name.clone());
                    constructors_ordering.push(constructor_decl.name.clone()); 
                    constructors.insert(constructor_decl.name.clone(), constructor_decl);
                }
                Self::Ind(IndDeclaration {
                    name: pre_decl.name,
                    recursive_type_var: pre_decl.recursive_type_var,
                    type_parameters: pre_decl.type_parameters,
                    constructors,
                    constructors_ordering,
                })
            }
        }
    }

    pub fn name(&self) -> &ConstructorName {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => &decl.name,
            Ind(decl) => &decl.name,
        }
    }

    pub fn arity(&self) -> usize {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => decl.type_parameters.len(),
            Ind(decl) => decl.type_parameters.len(),
        }
    }

    // WARNING: Assumes number of elements of `type_args` matches arity of the constructor.
    // e.g. consider type declaration
    //   type BinTree(a, b) = ind { tree .
    //   | Leaf(a)
    //   | Branch(b, Fn(Two -> tree))
    //   }
    // then type-applying the type arguments [Bool, Int] to the Branch constructor leads to
    //   Branch(Int, Fn(Two -> BinTree(Bool, Int)))
    pub fn type_apply_constructor(&self, constructor_name: &ConstructorName, type_args: &[Type]) -> Option<(&ConstructorDeclaration, Vec<Type>)> {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => decl.type_apply_constructor(constructor_name, type_args),
            Ind(decl) => decl.type_apply_constructor(constructor_name, type_args, &Type::TypeApplication(self.name().clone(), type_args.to_vec())),
        }
    }

    pub fn constructors(&self) -> &HashMap<ConstructorName, ConstructorDeclaration> {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => &decl.constructors,
            Ind(decl) => &decl.constructors,
        }
    }

    pub fn constructors_in_source_ordering(&self) -> Vec<&ConstructorDeclaration> {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => decl.constructors_in_source_ordering(),
            Ind(decl) => decl.constructors_in_source_ordering(),
        }
    }
}

impl EnumDeclaration {
    pub fn type_apply_constructor(&self, constructor_name: &ConstructorName, type_args: &[Type]) -> Option<(&ConstructorDeclaration, Vec<Type>)> {
        let constructor_decl = self.constructors.get(constructor_name)?;
        let specialized_constructor_type_arguments: Vec<Type> = constructor_decl.parameters
            .iter()
            .map(|type_body| type_apply(&self.type_parameters, type_body, type_args))
            .collect();
        Some((constructor_decl, specialized_constructor_type_arguments))
    }

    pub fn get_constructor(&self, constructor_name: &ConstructorName) -> Option<&ConstructorDeclaration> {
        self.constructors.get(constructor_name)
    }

    pub fn constructors_in_source_ordering(&self) -> Vec<&ConstructorDeclaration> {
        let mut result = vec![];
        for constructor_name in &self.constructors_ordering {
            let cons = self.get_constructor(constructor_name).unwrap();
            result.push(cons);
        }
        result
    }
}

impl IndDeclaration {
    pub fn type_apply_constructor(&self, constructor_name: &ConstructorName, type_args: &[Type], recursive_type: &Type) -> Option<(&ConstructorDeclaration, Vec<Type>)> {
        let constructor_decl = self.constructors.get(constructor_name)?;

        // TODO: This assumes that type_parameters + rec_type_var are all unique, and that
        // the rec_type_var doesn't shadow anything. But I don't yet check this!
        let mut type_parameters: Vec<Variable> = self.type_parameters.to_vec();
        type_parameters.push(self.recursive_type_var.clone());

        let mut type_args: Vec<Type> = type_args.to_vec();
        type_args.push(recursive_type.clone());

        let specialized_constructor_type_arguments: Vec<Type> = constructor_decl.parameters
            .iter()
            .map(|type_body| type_apply(&type_parameters, type_body, &type_args))
            .collect();
        Some((constructor_decl, specialized_constructor_type_arguments))
    }

    pub fn get_constructor(&self, constructor_name: &ConstructorName) -> Option<&ConstructorDeclaration> {
        self.constructors.get(constructor_name)
    }

    pub fn constructors_in_source_ordering(&self) -> Vec<&ConstructorDeclaration> {
        let mut result = vec![];
        for constructor_name in &self.constructors_ordering {
            let cons = self.get_constructor(constructor_name).unwrap();
            result.push(cons);
        }
        result
    }
}

impl FunctionDeclaration {
    pub fn name(&self) -> FunctionName {
        match self {
            Self::User(decl) => decl.name(),
            Self::Foreign(decl) => decl.name(),
        }
    }
}

impl UserFunctionDeclaration {
    pub fn name(&self) -> FunctionName {
        self.name.clone()
    }

    pub fn type_arity(&self) -> usize {
        self.type_parameters.len()
    }

    // Assumes arity matches.
    pub fn type_apply(&self, type_arguments: &[Type]) -> FunctionType {
        // TODO: This can be pretty space-expensive substitution. Consider having proper closures as type
        //       expressions.
        FunctionType {
            input_types: self.function.type_.input_types.iter().map(|type_| type_apply(&self.type_parameters, type_, type_arguments)).collect(),
            output_type: type_apply(&self.type_parameters, &self.function.type_.output_type, type_arguments)
        }
    }
}

impl ForeignFunctionDeclaration {
    pub fn name(&self) -> FunctionName {
        self.name.clone()
    }

    pub fn external_name(&self) -> ExternalName {
        self.external_name.clone()
    }
}

impl ConstructorDeclaration {
    pub fn arity(&self) -> usize { 
        self.parameters.len()
    }
}

impl Function {
    pub fn free_variables(&self) -> Vec<Variable> {
        let mut env: Env = Env::new();
        free_variables_in_function(&mut env, self);
        env.free_vars
    }
}

// ===Free Variables===
struct Env {
    scope_stack: Vec<HashSet<Variable>>,
    free_vars: Vec<Variable>,
    free_vars_set: HashSet<Variable>,
}

impl Env {
    fn new() -> Self {
        Self { scope_stack: vec![], free_vars: vec![], free_vars_set: HashSet::new() }
    }

    fn open(&mut self) {
        self.scope_stack.push(HashSet::new())
    }

    fn close(&mut self) {
        let _ = self.scope_stack.pop();
    }

    fn extend_var(&mut self, var: &Variable) {
        match self.scope_stack.last_mut() {
            Some(scope) => {
                let _ = scope.insert(var.clone());
            },
            None => unreachable!(),
        }
    }

    fn extend_vars(&mut self, vars: &[Variable]) {
        for var in vars {
            self.extend_var(var)
        }
    }

    fn extend_pattern(&mut self, pattern: &Pattern) { 
        use Pattern::*;
        match pattern {
            Constructor(_, patterns) => {
                for pattern in patterns {
                    self.extend_pattern(pattern)
                }
            },
            Variable(var) => {
                self.extend_var(var)
            },
            Int(_) => {},
            Anything(_) => {},
        }
    }

    fn is_bound(&self, var: &Variable) -> bool {
        for scope in self.scope_stack.iter().rev() {
            if scope.contains(var) {
                return true
            }
        }
        false
    }

    fn attempt_to_register_free(&mut self, var: &Variable) {
        if !(self.free_vars_set.contains(var) || self.is_bound(var)) {
            self.free_vars.push(var.clone());
            self.free_vars_set.insert(var.clone());
        } 
    }
}

fn free_variables_in_function(env: &mut Env, function: &Function) {
    env.open();
    env.extend_vars(&function.parameters);
    free_variables(env, &function.body);
    env.close();
}

fn free_variables(env: &mut Env, term: &Term) {
    use Term::*;

    match term {
        TypedTerm(typed_term) => free_variables(env, &typed_term.term),
        VariableUse(var) => {
            env.attempt_to_register_free(var)
        },
        Int(_) => {}, 
        FunctionApplication(_, _, args) => {
            for arg in args {
                free_variables(env, arg)
            }
        },
        ConstructorUse(_, args) => {
            for arg in args {
                free_variables(env, arg)
            }
        },
        Match(arg, branches) => {
            free_variables(env, arg);
            for branch in branches {
                env.open();
                env.extend_pattern(&branch.pattern);
                free_variables(env, &branch.body);
                env.close();
            }
        },
        Fold(arg, branches) => {
            free_variables(env, arg);
            for branch in branches {
                env.open();
                env.extend_pattern(&branch.pattern);
                free_variables(env, &branch.body);
                env.close();
            }
        },
        Lambda(function) => {
            free_variables_in_function(env, function)
        },
        LambdaApplication(fn_term, args) => {
            free_variables(env, fn_term);
            for arg in args {
                free_variables(env, arg)
            }
        },
        Let(bindings, body) => {
            env.open();
            for (var, term) in bindings {
                env.extend_var(var);
                free_variables(env, term);
            }
            free_variables(env, body);
            env.close();
        },
        Pure(term) => free_variables(env, term),
    }
}

impl Pattern {
    // Collect all the variables in the pattern
    pub fn variables(&self) -> Vec<Variable> {
        let mut vars: Vec<Variable> = vec![];

        fn pattern_loop(pattern: &Pattern, vars: &mut Vec<Variable>) {
            use Pattern::*;
            match pattern {
                Constructor(_, patterns) => {
                    for pattern in patterns {
                        pattern_loop(pattern, vars)
                    }
                },
                Variable(variable) => {
                    vars.push(variable.clone())
                },
                Int(_) => {},
                Anything(_) => {},
            }
        }
        pattern_loop(self, &mut vars);

        vars
    }
}

// ===Type Application===
pub fn type_apply(type_parameters: &[Variable], type_body: &Type, type_arguments: &[Type]) -> Type {
    // TODO: What are the assumptions? Am I responsible for checking the arities are correct here?
    //
    // given a variable in type_body, I need to know which argument to substitute...
    // So I actually need to have Variable ~> index mapping...
    //
    // This is not very efficient.
    let mut position_map: HashMap<&Variable, usize> = HashMap::new();
    for (i, var) in type_parameters.iter().enumerate() {
        position_map.insert(var, i);
    }
    
    fn traverse(position_map: &HashMap<&Variable, usize>, type_body: &Type, type_arguments: &[Type]) -> Type {
        use Type::*;
        match type_body {
            VariableUse(var) => {
                match position_map.get(var) {
                    Some(i) => type_arguments[*i].clone(),
                    None => unreachable!(),
                }
            },
            TypeApplication(type_name, args) => {
                let applied_args: Vec<Type> = args.into_iter().map(|type_| traverse(position_map, type_, type_arguments)).collect();
                TypeApplication(type_name.clone(), applied_args)
            }
            Arrow(fn_type) => {
                let applied_input_types: Vec<Type> = fn_type.input_types.iter().map(|type_| traverse(position_map, type_, type_arguments)).collect();
                let applied_output_type = traverse(position_map, &fn_type.output_type, type_arguments);
                Arrow(Box::new(FunctionType { input_types: applied_input_types, output_type: applied_output_type }))
            },
            I32 => I32,
            Command(type_body) => Command(Box::new(traverse(position_map, type_body, type_arguments))),
        }
    }

    traverse(&position_map, type_body, type_arguments)
}
