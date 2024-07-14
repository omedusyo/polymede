use crate::parser::lex::{
    lexer,
    lexer::{Request, LocatedToken, DeclarationKind},
    token::Keyword,
};
use crate::parser::{
    program::pre_program,
    identifier::{Interner, interner, Variable, ConstructorName, FunctionName, Identifier}
};
use std::collections::HashMap;

pub type Result<A> = std::result::Result<A, Error>;

// TODO: Consider moving the type definitions out of the parsing module, since this seems to be
// shared between checking and parsing.

// Note that this is not a closure type. It's simply a function pointer.
pub type Parser<A> = fn(&mut State) -> Result<A>;

#[derive(Debug)]
pub enum Error {
    LexError(lexer::Error),
    ExpectedTypeConstructorOrTypeVar { received: Identifier },
    ExpectedTypeConstructorOrTypeVarOrAnythingInPattern { received: Identifier },
    ExpectedTerm { received: Identifier },
    ExpectedTypeConstructor { received: Identifier },
    ExpectedTypeVar { received: Identifier },
    DuplicateVariableNames { duplicates: Vec<Identifier> },
    // Atleast one vector is non-empty.
    DuplicateNames {
        type_duplicates: Vec<Identifier>,
        constructor_duplicates: Vec<Identifier>,
        function_duplicates: Vec<Identifier>,
        let_duplicates: Vec<Identifier>
    },
    FunctionHasDifferentNumberOfParametersThanDeclaredInItsType { declared_in_type: usize, parameters: usize },
}


#[derive(Debug)]
pub struct State<'lex_state, 'interner> {
    lexer_state: lexer::State<'lex_state>,
    interner: &'interner mut Interner,
}

// ===Program===
#[derive(Debug)]
pub struct Program {
    interner: Interner,
    pub type_declarations: HashMap<ConstructorName, TypeDeclaration>,
    pub function_declarations: HashMap<FunctionName, FunctionDeclaration>,
    pub let_declarations: HashMap<Variable, LetDeclaration>,
    constructor_to_type_mapping: HashMap<ConstructorName, Variable>,
}

impl Program {
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    pub fn parse(s: &str) -> Result<Self> {
        let mut interner: Interner = interner();
        let mut state = State::new(s, &mut interner);
        let pre_program = pre_program(&mut state)?;
        let mut program = Self {
            interner,
            type_declarations: HashMap::new(),
            function_declarations: HashMap::new(),
            let_declarations: HashMap::new(),
            constructor_to_type_mapping: HashMap::new(),
        };

        for pre_decl in pre_program.type_declarations {
            let decl = TypeDeclaration::new(pre_decl,  &mut program.constructor_to_type_mapping);
            program.type_declarations.insert(decl.name().clone(), decl);
        }
        for decl in pre_program.function_declarations {
            program.function_declarations.insert(decl.name(), decl);
        }
        for decl in pre_program.let_declarations {
            program.let_declarations.insert(decl.name(), decl);
        }

        Ok(program)
    }

    pub fn get_type_declaration(&self, type_name: &Variable) -> Option<&TypeDeclaration> {
        self.type_declarations.get(type_name)
    }

    pub fn get_function_declaration(&self, function_name: &FunctionName) -> Option<&FunctionDeclaration> {
        self.function_declarations.get(function_name)
    }

    pub fn get_let_declaration(&self, let_name: &FunctionName) -> Option<&LetDeclaration> {
        self.let_declarations.get(let_name)
    }

    pub fn get_type_declaration_of_constructor(&self, constructor_name: &ConstructorName) -> Option<&TypeDeclaration> {
        let type_name = self.constructor_to_type_mapping.get(constructor_name)?;
        self.get_type_declaration_of_constructor(type_name)
    }
}


#[derive(Debug)]
pub struct PreProgram {
    pub type_declarations: Vec<PreTypeDeclaration>,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub let_declarations: Vec<LetDeclaration>,
}

// ===Declarations===
pub enum Declaration {
    Type(PreTypeDeclaration),
    Let(LetDeclaration),
    Function(FunctionDeclaration),
}

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
pub struct PreEnumDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub struct IndDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub recursive_type_var: Variable,
    pub constructors: HashMap<ConstructorName, ConstructorDeclaration>,
}

#[derive(Debug)]
pub struct PreIndDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub recursive_type_var: Variable,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub enum TypeDeclaration {
    Enum(EnumDeclaration),
    Ind(IndDeclaration),
}

#[derive(Debug)]
pub enum PreTypeDeclaration {
    Enum(PreEnumDeclaration),
    Ind(PreIndDeclaration),
}

impl EnumDeclaration {
    // Returns type_parameters together with declaration.
    pub fn get_constructor(&self, constructor_name: &ConstructorName) -> Option<(&[Variable], &ConstructorDeclaration)> {
        let constructor_decl = self.constructors.get(constructor_name)?;
        Some((&self.type_parameters, constructor_decl))
    }
}

impl IndDeclaration {
    // Returns type_parameters (and a special recursive variable) together with declaration.
    pub fn get_constructor(&self, constructor_name: &ConstructorName) -> Option<(&[Variable], &Variable, &ConstructorDeclaration)> {
        let constructor_decl = self.constructors.get(constructor_name)?;
        Some((&self.type_parameters, &self.recursive_type_var, constructor_decl))
    }
}

impl TypeDeclaration {
    fn new(pre_decl: PreTypeDeclaration, constructor_to_type_mapping: &mut HashMap<ConstructorName, Variable>) -> Self {
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

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: FunctionName,
    pub type_parameters: Vec<Variable>,
    pub function: TypedFunction,
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

impl LetDeclaration {
    pub fn name(&self) -> Variable {
        self.name.clone()
    }
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

impl PreProgram {
    pub fn new() -> Self {
        Self { type_declarations: vec![], function_declarations: vec![], let_declarations: vec![] }
    }

    pub fn add_declaration(&mut self, decl: Declaration) {
        match decl {
            Declaration::Type(type_declaration) => {
                self.type_declarations.push(type_declaration)
            },
            Declaration::Let(let_declaration) => {
                self.let_declarations.push(let_declaration)
            },
            Declaration::Function(function_declaration) => {
                self.function_declarations.push(function_declaration)
            },
        }
    }

    pub fn type_names(&self) -> Vec<Variable> {
        let mut names = vec![];
        for declaration in &self.type_declarations {
            match declaration {
                PreTypeDeclaration::Enum(declaration) => {
                    names.push(declaration.name.clone())
                },
                PreTypeDeclaration::Ind(declaration) => {
                    names.push(declaration.name.clone())
                },
            }
        }
        names
    }

    pub fn constructor_names(&self) -> Vec<ConstructorName> {
        let mut names = vec![];
        for declaration in &self.type_declarations {
            let constructors = match &declaration {
                PreTypeDeclaration::Enum(declaration) => &declaration.constructors[..],
                PreTypeDeclaration::Ind(declaration) => &declaration.constructors[..],
            };
            for constructor_declaration in constructors {
                names.push(constructor_declaration.name.clone())
            }
        }
        names
    }

    pub fn function_names(&self) -> Vec<FunctionName> {
        let mut names = vec![];
        for declaration in &self.function_declarations {
            names.push(declaration.name.clone())
        }
        names
    }

    pub fn let_names(&self) -> Vec<Variable> {
        let mut names = vec![];
        for declaration in &self.let_declarations {
            names.push(declaration.name.clone())
        }
        names
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

impl <'lex_state, 'interner> State<'lex_state, 'interner> {
    pub fn new<'a: 'lex_state>(str: &'a str, interner: &'interner mut Interner) -> Self {
        Self {
            interner,
            lexer_state: lexer::State::new(str)
        }
    }

    pub fn interner(&mut self) -> &mut Interner {
        &mut self.interner
    }

    pub fn request_token(&mut self, request: Request) -> Result<LocatedToken> {
        self.lexer_state.request(request).map_err(Error::LexError)
    }

    pub fn request_keyword(&mut self, keyword: Keyword) -> Result<LocatedToken> {
        self.request_token(Request::Keyword(keyword))
    }

    pub fn consume_optional_or(&mut self) -> Result<()> {
        self.lexer_state.consume_optional_or().map_err(Error::LexError)
    }

    pub fn consume_whitespace_or_fail_when_end(&mut self) -> Result<()> {
        self.lexer_state.consume_whitespace_or_fail_when_end().map_err(Error::LexError)
    }

    pub fn consume_optional_comma(&mut self) -> Result<()> {
        self.lexer_state.consume_optional_comma().map_err(Error::LexError)
    }

    pub fn is_next_token_open_paren(&mut self) -> Result<bool> {
        self.lexer_state.is_next_token_open_paren().map_err(Error::LexError)
    }

    pub fn is_next_token_open_angle(&mut self) -> Result<bool> {
        self.lexer_state.is_next_token_open_angle().map_err(Error::LexError)
    }

    pub fn is_next_token_start_type_annotation(&mut self) -> Result<bool> {
        self.lexer_state.is_next_token_start_type_annotation().map_err(Error::LexError)
    }

    pub fn commit_if_next_token_forall(&mut self) -> Result<bool> {
        self.lexer_state.commit_if_next_token_forall().map_err(Error::LexError)
    }

    pub fn peek_declaration_token(&mut self) -> Result<DeclarationKind> {
        self.lexer_state.peek_declaration_token().map_err(Error::LexError)
    }

    pub fn clone(&self) -> lexer::State<'lex_state> {
        self.lexer_state.clone()
    }

    pub fn restore(&mut self, lexer_state: lexer::State<'lex_state>) {
        self.lexer_state = lexer_state;
    }
}
