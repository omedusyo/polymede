use crate::identifier::{Interner, interner, Variable, ConstructorName, FunctionName, Identifier};
use crate::parser::base::PreTypeDeclaration;
use std::collections::HashMap;

// ===Program===
#[derive(Debug)]
pub struct Program {
    interner: Interner,
    pub type_declarations: HashMap<ConstructorName, TypeDeclaration>,
    pub function_declarations: HashMap<FunctionName, FunctionDeclaration>,
    pub let_declarations: HashMap<Variable, LetDeclaration>,
    pub constructor_to_type_mapping: HashMap<ConstructorName, Variable>,
}

impl Program {
    pub fn new() -> Self {
        Self { 
            interner: interner(),
            type_declarations: HashMap::new(),
            function_declarations: HashMap::new(),
            let_declarations: HashMap::new(),
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
        self.get_type_declaration_of_constructor(type_name)
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
}

#[derive(Debug)]
pub struct IndDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub recursive_type_var: Variable,
    pub constructors: HashMap<ConstructorName, ConstructorDeclaration>,
}

#[derive(Debug)]
pub enum TypeDeclaration {
    Enum(EnumDeclaration),
    Ind(IndDeclaration),
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: FunctionName,
    pub type_parameters: Vec<Variable>,
    pub function: TypedFunction,
}

#[derive(Debug)]
pub struct TypedFunction {
    pub type_: FunctionType,
    pub function: Function,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Variable>,
    pub body: Term,
}

// TODO: Get rid of this or replace it by a version that doesn't have type parameters.
#[derive(Debug)]
pub struct LetDeclaration {
    pub name: Variable,
    pub type_parameters: Vec<Variable>,
    pub body: TypedTerm,
}

// ===Types===
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    VariableUse(Variable),
    TypeApplication(ConstructorName, Vec<Type>),
    Arrow(Box<FunctionType>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub input_types: Vec<Type>,
    pub output_type: Type,
}


// ===Terms===
#[derive(Debug)]
pub struct TypedTerm {
    pub type_: Type,
    pub term: Term
}

#[derive(Debug)]
pub enum Term {
    TypedTerm(Box<TypedTerm>),
    VariableUse(Variable),
    FunctionApplication(Variable, Vec<Type>, Vec<Term>),
    ConstructorUse(ConstructorName, Vec<Term>),
    Match(Box<Term>, Vec<PatternBranch>),
    Fold(Box<Term>, Vec<PatternBranch>),
    Lambda(Box<Function>),
    LambdaApplication(Box<Term>, Vec<Term>),
    Let(Vec<(Variable, Term)>, Box<Term>),
}

#[derive(Debug)]
pub struct PatternBranch {
    pub pattern: Pattern,
    pub body: Term,
}

#[derive(Debug)]
pub enum Pattern {
    Constructor(ConstructorName, Vec<Pattern>),
    Variable(Variable),
    Anything(Identifier),
}

impl TypeDeclaration {
    pub fn new(pre_decl: PreTypeDeclaration, constructor_to_type_mapping: &mut HashMap<ConstructorName, Variable>) -> Self {
        match pre_decl {
            PreTypeDeclaration::Enum(pre_decl) => {
                let mut constructors = HashMap::new();
                for constructor_decl in pre_decl.constructors {
                    constructor_to_type_mapping.insert(constructor_decl.name.clone(), pre_decl.name.clone());
                    constructors.insert(constructor_decl.name.clone(), constructor_decl);
                }
                Self::Enum(EnumDeclaration {
                    name: pre_decl.name,
                    type_parameters: pre_decl.type_parameters,
                    constructors,
                })
            },
            PreTypeDeclaration::Ind(pre_decl) => {
                let mut constructors = HashMap::new();
                for constructor_decl in pre_decl.constructors {
                    constructor_to_type_mapping.insert(constructor_decl.name.clone(), pre_decl.name.clone());
                    constructors.insert(constructor_decl.name.clone(), constructor_decl);
                }
                Self::Ind(IndDeclaration {
                    name: pre_decl.name,
                    recursive_type_var: pre_decl.recursive_type_var,
                    type_parameters: pre_decl.type_parameters,
                    constructors,
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
}

impl FunctionDeclaration {
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

impl LetDeclaration {
    pub fn name(&self) -> Variable {
        self.name.clone()
    }
}

impl ConstructorDeclaration {
    pub fn arity(&self) -> usize { 
        self.parameters.len()
    }
}

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
        }
    }

    traverse(&position_map, type_body, type_arguments)
}
