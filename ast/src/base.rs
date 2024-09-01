use crate::identifier::{
    interner, ConstructorName, ExternalName, FunctionName, Identifier, Interner, Variable,
};
use crate::parser::base::PreTypeDeclaration;
use std::collections::HashMap;

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
    pub msg_type: Option<Variable>,
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
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
            msg_type: None,
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

    pub fn get_function_declaration(
        &self,
        function_name: &FunctionName,
    ) -> Option<&FunctionDeclaration> {
        self.function_declarations.get(function_name)
    }

    pub fn get_type_declaration_of_constructor(
        &self,
        constructor_name: &ConstructorName,
    ) -> Option<&TypeDeclaration> {
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

    pub fn get_msg_type_declaration(&self) -> &TypeDeclaration {
        let Some(msg_type_name) = &self.msg_type else {
            unreachable!()
        };
        let Some(msg_type_declaration) = self.get_type_declaration(msg_type_name) else {
            unreachable!()
        };
        msg_type_declaration
    }

    pub fn get_msg_type(&self) -> Type {
        let decl = self.get_msg_type_declaration();
        let type_name = decl.name();
        Type::TypeApplication(type_name.clone(), vec![])
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
    #[allow(clippy::enum_variant_names)]
    TypeApplication(Variable, Vec<Type>),
    Arrow(Box<FunctionType>),
    I32,
    F32,
    String,
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
    pub term: Term,
}

#[derive(Debug, Clone)]
pub enum Term {
    Typed(Box<TypedTerm>),
    Int(i32),
    Float(f32),
    StringLiteral(String),
    VariableUse(Variable),
    FunctionApplication(FunctionName, Vec<Type>, Vec<Term>),
    ConstructorUse(ConstructorName, Vec<Term>),
    Match(Box<Term>, Vec<PatternBranch>),
    Fold(Box<Term>, Vec<PatternBranch>),
    Lambda(Box<Function>),
    LambdaApplication(Box<Term>, Vec<Term>),
    Let(Vec<(Variable, Term)>, Box<Term>),
    Pure(Box<Term>),
    Do(Vec<DoBinding>, Box<Term>),
    Receive,
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

#[derive(Debug, Clone)]
pub enum DoBinding {
    ExecuteThenBind(Variable, Term),
    Bind(Variable, Term),
}

impl TypeDeclaration {
    pub fn new(
        pre_decl: PreTypeDeclaration,
        constructor_to_type_mapping: &mut HashMap<ConstructorName, Variable>,
    ) -> Self {
        match pre_decl {
            PreTypeDeclaration::Enum(pre_decl) => {
                let mut constructors = HashMap::new();
                let mut constructors_ordering = vec![];
                for constructor_decl in pre_decl.constructors {
                    constructor_to_type_mapping
                        .insert(constructor_decl.name.clone(), pre_decl.name.clone());
                    constructors_ordering.push(constructor_decl.name.clone());
                    constructors.insert(constructor_decl.name.clone(), constructor_decl);
                }
                Self::Enum(EnumDeclaration {
                    name: pre_decl.name,
                    type_parameters: pre_decl.type_parameters,
                    constructors,
                    constructors_ordering,
                })
            }
            PreTypeDeclaration::Ind(pre_decl) => {
                let mut constructors = HashMap::new();
                let mut constructors_ordering = vec![];
                for constructor_decl in pre_decl.constructors {
                    constructor_to_type_mapping
                        .insert(constructor_decl.name.clone(), pre_decl.name.clone());
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
    pub fn type_apply_constructor(
        &self,
        constructor_name: &ConstructorName,
        type_args: &[Type],
    ) -> Option<(&ConstructorDeclaration, Vec<Type>)> {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => decl.type_apply_constructor(constructor_name, type_args),
            Ind(decl) => decl.type_apply_constructor(
                constructor_name,
                type_args,
                &Type::TypeApplication(self.name().clone(), type_args.to_vec()),
            ),
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

impl Type {
    // Checks: if type parameters are value types, then the type is also a value type.
    pub fn is_value_type(&self) -> bool {
        use Type::*;
        match self {
            VariableUse(_) => true,
            TypeApplication(_type_name, types) => {
                // TODO: This is not sufficient. We need to have access to the declaration of
                // `_type_name` and that needs to be a value type too. Would be nice if this
                // information was precomputed. Also you need some sort of loop check for mutually
                // recursive types.
                for type_ in types {
                    if type_.is_value_type() {
                        return false;
                    }
                }
                true
            }
            Arrow(_) => false,
            I32 => true,
            F32 => true,
            String => true,
            Command(_) => false,
        }
    }
}

impl EnumDeclaration {
    pub fn type_apply_constructor(
        &self,
        constructor_name: &ConstructorName,
        type_args: &[Type],
    ) -> Option<(&ConstructorDeclaration, Vec<Type>)> {
        let constructor_decl = self.constructors.get(constructor_name)?;
        let specialized_constructor_type_arguments: Vec<Type> = constructor_decl
            .parameters
            .iter()
            .map(|type_body| type_apply(&self.type_parameters, type_body, type_args))
            .collect();
        Some((constructor_decl, specialized_constructor_type_arguments))
    }

    pub fn get_constructor(
        &self,
        constructor_name: &ConstructorName,
    ) -> Option<&ConstructorDeclaration> {
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
    pub fn type_apply_constructor(
        &self,
        constructor_name: &ConstructorName,
        type_args: &[Type],
        recursive_type: &Type,
    ) -> Option<(&ConstructorDeclaration, Vec<Type>)> {
        let constructor_decl = self.constructors.get(constructor_name)?;

        // TODO: This assumes that type_parameters + rec_type_var are all unique, and that
        // the rec_type_var doesn't shadow anything. But I don't yet check this!
        let mut type_parameters: Vec<Variable> = self.type_parameters.to_vec();
        type_parameters.push(self.recursive_type_var.clone());

        let mut type_args: Vec<Type> = type_args.to_vec();
        type_args.push(recursive_type.clone());

        let specialized_constructor_type_arguments: Vec<Type> = constructor_decl
            .parameters
            .iter()
            .map(|type_body| type_apply(&type_parameters, type_body, &type_args))
            .collect();
        Some((constructor_decl, specialized_constructor_type_arguments))
    }

    pub fn get_constructor(
        &self,
        constructor_name: &ConstructorName,
    ) -> Option<&ConstructorDeclaration> {
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
            input_types: self
                .function
                .type_
                .input_types
                .iter()
                .map(|type_| type_apply(&self.type_parameters, type_, type_arguments))
                .collect(),
            output_type: type_apply(
                &self.type_parameters,
                &self.function.type_.output_type,
                type_arguments,
            ),
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
                }
                Variable(variable) => vars.push(variable.clone()),
                Int(_) => {}
                Anything(_) => {}
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

    fn traverse(
        position_map: &HashMap<&Variable, usize>,
        type_body: &Type,
        type_arguments: &[Type],
    ) -> Type {
        use Type::*;
        match type_body {
            VariableUse(var) => match position_map.get(var) {
                Some(i) => type_arguments[*i].clone(),
                None => unreachable!(),
            },
            TypeApplication(type_name, args) => {
                let applied_args: Vec<Type> = args
                    .iter()
                    .map(|type_| traverse(position_map, type_, type_arguments))
                    .collect();
                TypeApplication(type_name.clone(), applied_args)
            }
            Arrow(fn_type) => {
                let applied_input_types: Vec<Type> = fn_type
                    .input_types
                    .iter()
                    .map(|type_| traverse(position_map, type_, type_arguments))
                    .collect();
                let applied_output_type =
                    traverse(position_map, &fn_type.output_type, type_arguments);
                Arrow(Box::new(FunctionType {
                    input_types: applied_input_types,
                    output_type: applied_output_type,
                }))
            }
            I32 => I32,
            F32 => F32,
            String => String,
            Command(type_body) => {
                Command(Box::new(traverse(position_map, type_body, type_arguments)))
            }
        }
    }

    traverse(&position_map, type_body, type_arguments)
}
