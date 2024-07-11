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
            program.type_declarations.insert(decl.name(), decl);
        }
        for decl in pre_program.function_declarations {
            program.function_declarations.insert(decl.name(), decl);
        }
        for decl in pre_program.let_declarations {
            program.let_declarations.insert(decl.name(), decl);
        }

        Ok(program)
    }

    pub fn get_type_declaration(&self, type_constructor_name: &ConstructorName) -> Option<&TypeDeclaration> {
        self.type_declarations.get(type_constructor_name)
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

    fn name(&self) -> ConstructorName {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => decl.name.clone(),
            Ind(decl) => decl.name.clone(),
        }
    }

    pub fn arity(&self) -> usize {
        use TypeDeclaration::*;
        match self {
            Enum(decl) => decl.type_parameters.len(),
            Ind(decl) => decl.type_parameters.len(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: FunctionName,
    pub type_parameters: Vec<Variable>,
    pub function: Function,
}

impl FunctionDeclaration {
    fn name(&self) -> FunctionName {
        self.name.clone()
    }
}

#[derive(Debug)]
pub struct Function {
    pub type_: FunctionType,
    pub parameters: Vec<Variable>,
    pub body: Term,
}

#[derive(Debug)]
pub struct LetDeclaration {
    pub name: Variable,
    pub type_parameters: Vec<Variable>,
    pub body: TypedTerm,
}

impl LetDeclaration {
    fn name(&self) -> Variable {
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
    FunctionApplication(Variable, Vec<Term>),
    ConstructorUse(ConstructorName, Vec<Term>),
    Match(Box<Term>, Vec<PatternBranch>),
    Fold(Box<Term>, Vec<PatternBranch>),
    Lambda(Box<Function>),
    LambdaApplication(Box<Term>, Vec<Term>),
    Let(Vec<(Variable, TypedTerm)>, Box<Term>),
}

#[derive(Debug)]
pub struct LocalLetBinding {
    pub bindings: Vec<(Variable, TypedTerm)>,
    pub body: Term,
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
