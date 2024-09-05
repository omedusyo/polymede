use crate::identifier::{
    interner, ConstructorName, ExternalName, FunctionName, Interner, RawIdentifier, TypeName,
    TypeVariable, Variable,
};
use crate::parser::base::PreTypeDefinition;
use std::collections::HashMap;

// ===Program===
#[derive(Debug)]
pub struct Program {
    interner: Interner,
    pub type_definitions: HashMap<TypeName, TypeDefinition>,
    pub type_definitions_ordering: Vec<TypeName>,
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
    pub run_definition: Option<RunDefinition>,
    pub constructor_to_type_mapping: HashMap<ConstructorName, TypeName>,
    pub msg_type: Option<TypeName>,
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
            type_definitions: HashMap::new(),
            type_definitions_ordering: vec![],
            function_definitions: HashMap::new(),
            function_definitions_ordering: vec![],
            run_definition: None,
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

    pub fn get_type_definition(&self, type_name: &TypeName) -> Option<&TypeDefinition> {
        self.type_definitions.get(type_name)
    }

    pub fn get_function_definition(
        &self,
        function_name: &FunctionName,
    ) -> Option<&FunctionDefinition> {
        self.function_definitions.get(function_name)
    }

    pub fn get_type_definition_of_constructor(
        &self,
        constructor_name: &ConstructorName,
    ) -> Option<&TypeDefinition> {
        let type_name = self.constructor_to_type_mapping.get(constructor_name)?;
        self.get_type_definition(type_name)
    }

    pub fn type_definition_in_source_ordering(&self) -> Vec<&TypeDefinition> {
        let mut result = vec![];
        for type_name in &self.type_definitions_ordering {
            result.push(self.get_type_definition(type_name).unwrap())
        }
        result
    }

    pub fn get_msg_type_definition(&self) -> &TypeDefinition {
        let Some(msg_type_name) = &self.msg_type else {
            unreachable!()
        };
        let Some(msg_type_definition) = self.get_type_definition(msg_type_name) else {
            unreachable!()
        };
        msg_type_definition
    }

    pub fn get_msg_type(&self) -> Type {
        let def = self.get_msg_type_definition();
        let type_name = def.name();
        Type::TypeApplication(type_name.clone(), vec![])
    }

    pub fn function_definitions_in_source_ordering(&self) -> Vec<&FunctionDefinition> {
        let mut result = vec![];
        for function_name in &self.function_definitions_ordering {
            result.push(self.get_function_definition(function_name).unwrap())
        }
        result
    }
}

// ===Definitions===
#[derive(Debug)]
pub struct ConstructorDefinition {
    pub name: ConstructorName,
    pub parameters: Vec<Type>,
}

#[derive(Debug)]
pub struct EnumDefinition {
    pub name: TypeName,
    pub type_parameters: Vec<TypeVariable>,
    pub constructors: HashMap<ConstructorName, ConstructorDefinition>,
    pub constructors_ordering: Vec<ConstructorName>,
}

#[derive(Debug)]
pub struct IndDefinition {
    pub name: TypeName,
    pub type_parameters: Vec<TypeVariable>,
    pub recursive_type_var: TypeVariable,
    pub constructors: HashMap<ConstructorName, ConstructorDefinition>,
    pub constructors_ordering: Vec<ConstructorName>,
}

#[derive(Debug)]
pub enum TypeDefinition {
    Enum(EnumDefinition),
    Ind(IndDefinition),
}

#[derive(Debug)]
pub enum FunctionDefinition {
    User(UserFunctionBinding),
    Foreign(ForeignFunctionBinding),
}

#[derive(Debug)]
pub struct UserFunctionBinding {
    pub name: FunctionName,
    pub type_parameters: Vec<TypeVariable>,
    pub function: TypedFunction,
}

#[derive(Debug)]
pub struct ForeignFunctionBinding {
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
pub struct RunDefinition {
    pub body: TypedTerm,
}

// ===Types===
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    VariableUse(TypeVariable),
    #[allow(clippy::enum_variant_names)]
    TypeApplication(TypeName, Vec<Type>),
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
    Anything(RawIdentifier),
}

#[derive(Debug, Clone)]
pub enum DoBinding {
    ExecuteThenBind(Variable, Term),
    Bind(Variable, Term),
}

impl TypeDefinition {
    pub fn new(
        pre_def: PreTypeDefinition,
        constructor_to_type_mapping: &mut HashMap<ConstructorName, TypeName>,
    ) -> Self {
        match pre_def {
            PreTypeDefinition::Enum(pre_def) => {
                let mut constructors = HashMap::new();
                let mut constructors_ordering = vec![];
                for constructor_def in pre_def.constructors {
                    constructor_to_type_mapping
                        .insert(constructor_def.name.clone(), pre_def.name.clone());
                    constructors_ordering.push(constructor_def.name.clone());
                    constructors.insert(constructor_def.name.clone(), constructor_def);
                }
                Self::Enum(EnumDefinition {
                    name: pre_def.name,
                    type_parameters: pre_def.type_parameters,
                    constructors,
                    constructors_ordering,
                })
            }
            PreTypeDefinition::Ind(pre_def) => {
                let mut constructors = HashMap::new();
                let mut constructors_ordering = vec![];
                for constructor_def in pre_def.constructors {
                    constructor_to_type_mapping
                        .insert(constructor_def.name.clone(), pre_def.name.clone());
                    constructors_ordering.push(constructor_def.name.clone());
                    constructors.insert(constructor_def.name.clone(), constructor_def);
                }
                Self::Ind(IndDefinition {
                    name: pre_def.name,
                    recursive_type_var: pre_def.recursive_type_var,
                    type_parameters: pre_def.type_parameters,
                    constructors,
                    constructors_ordering,
                })
            }
        }
    }

    pub fn name(&self) -> &TypeName {
        use TypeDefinition::*;
        match self {
            Enum(def) => &def.name,
            Ind(def) => &def.name,
        }
    }

    pub fn arity(&self) -> usize {
        use TypeDefinition::*;
        match self {
            Enum(def) => def.type_parameters.len(),
            Ind(def) => def.type_parameters.len(),
        }
    }

    // WARNING: Assumes number of elements of `type_args` matches arity of the constructor.
    // e.g. consider type definitions
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
    ) -> Option<(&ConstructorDefinition, Vec<Type>)> {
        use TypeDefinition::*;
        match self {
            Enum(def) => def.type_apply_constructor(constructor_name, type_args),
            Ind(def) => def.type_apply_constructor(
                constructor_name,
                type_args,
                &Type::TypeApplication(self.name().clone(), type_args.to_vec()),
            ),
        }
    }

    pub fn constructors(&self) -> &HashMap<ConstructorName, ConstructorDefinition> {
        use TypeDefinition::*;
        match self {
            Enum(def) => &def.constructors,
            Ind(def) => &def.constructors,
        }
    }

    pub fn constructors_in_source_ordering(&self) -> Vec<&ConstructorDefinition> {
        use TypeDefinition::*;
        match self {
            Enum(def) => def.constructors_in_source_ordering(),
            Ind(def) => def.constructors_in_source_ordering(),
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
                // TODO: This is not sufficient. We need to have access to the definition of
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

impl EnumDefinition {
    pub fn type_apply_constructor(
        &self,
        constructor_name: &ConstructorName,
        type_args: &[Type],
    ) -> Option<(&ConstructorDefinition, Vec<Type>)> {
        let constructor_def = self.constructors.get(constructor_name)?;
        let specialized_constructor_type_arguments: Vec<Type> = constructor_def
            .parameters
            .iter()
            .map(|type_body| type_apply(&self.type_parameters, type_body, type_args))
            .collect();
        Some((constructor_def, specialized_constructor_type_arguments))
    }

    pub fn get_constructor(
        &self,
        constructor_name: &ConstructorName,
    ) -> Option<&ConstructorDefinition> {
        self.constructors.get(constructor_name)
    }

    pub fn constructors_in_source_ordering(&self) -> Vec<&ConstructorDefinition> {
        let mut result = vec![];
        for constructor_name in &self.constructors_ordering {
            let cons = self.get_constructor(constructor_name).unwrap();
            result.push(cons);
        }
        result
    }
}

impl IndDefinition {
    pub fn type_apply_constructor(
        &self,
        constructor_name: &ConstructorName,
        type_args: &[Type],
        recursive_type: &Type,
    ) -> Option<(&ConstructorDefinition, Vec<Type>)> {
        let constructor_def = self.constructors.get(constructor_name)?;

        // TODO: This assumes that type_parameters + rec_type_var are all unique, and that
        // the rec_type_var doesn't shadow anything. But I don't yet check this!
        let mut type_parameters: Vec<TypeVariable> = self.type_parameters.to_vec();
        type_parameters.push(self.recursive_type_var.clone());

        let mut type_args: Vec<Type> = type_args.to_vec();
        type_args.push(recursive_type.clone());

        let specialized_constructor_type_arguments: Vec<Type> = constructor_def
            .parameters
            .iter()
            .map(|type_body| type_apply(&type_parameters, type_body, &type_args))
            .collect();
        Some((constructor_def, specialized_constructor_type_arguments))
    }

    pub fn get_constructor(
        &self,
        constructor_name: &ConstructorName,
    ) -> Option<&ConstructorDefinition> {
        self.constructors.get(constructor_name)
    }

    pub fn constructors_in_source_ordering(&self) -> Vec<&ConstructorDefinition> {
        let mut result = vec![];
        for constructor_name in &self.constructors_ordering {
            let cons = self.get_constructor(constructor_name).unwrap();
            result.push(cons);
        }
        result
    }
}

impl FunctionDefinition {
    pub fn name(&self) -> FunctionName {
        match self {
            Self::User(def) => def.name(),
            Self::Foreign(def) => def.name(),
        }
    }
}

impl UserFunctionBinding {
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

impl ForeignFunctionBinding {
    pub fn name(&self) -> FunctionName {
        self.name.clone()
    }

    pub fn external_name(&self) -> ExternalName {
        self.external_name.clone()
    }
}

impl ConstructorDefinition {
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
pub fn type_apply(
    type_parameters: &[TypeVariable],
    type_body: &Type,
    type_arguments: &[Type],
) -> Type {
    // TODO: What are the assumptions? Am I responsible for checking the arities are correct here?
    //
    // given a variable in type_body, I need to know which argument to substitute...
    // So I actually need to have Variable ~> index mapping...
    //
    // This is not very efficient.
    let mut position_map: HashMap<&TypeVariable, usize> = HashMap::new();
    for (i, var) in type_parameters.iter().enumerate() {
        position_map.insert(var, i);
    }

    fn traverse(
        position_map: &HashMap<&TypeVariable, usize>,
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
