use crate::parser::lex::{
    lexer,
    lexer::{Request, LocatedToken, DeclarationKind},
    token::Keyword,
};
use crate::parser::identifier::{Variable, ConstructorName, FunctionName, Identifier};

pub type Result<A> = std::result::Result<A, Error>;

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
pub struct State<'a> {
    lexer_state: lexer::State<'a>
}

// ===Program===
#[derive(Debug)]
pub struct Program {
    pub type_declarations: Vec<TypeDeclaration>,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub let_declarations: Vec<LetDeclaration>,
}

// ===Declarations===
pub enum Declaration {
    Type(TypeDeclaration),
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
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub struct IndDeclaration {
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
pub struct FunctionDeclaration {
    pub name: FunctionName,
    pub type_parameters: Vec<Variable>,
    pub function: Function,
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

// ===Types===
#[derive(Debug, PartialEq)]
pub enum Type {
    VariableUse(Variable),
    TypeApplication(ConstructorName, Vec<Type>),
    Arrow(Box<FunctionType>),
}

#[derive(Debug, PartialEq)]
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

impl Program {
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
                TypeDeclaration::Enum(declaration) => {
                    names.push(declaration.name.clone())
                },
                TypeDeclaration::Ind(declaration) => {
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
                TypeDeclaration::Enum(declaration) => &declaration.constructors[..],
                TypeDeclaration::Ind(declaration) => &declaration.constructors[..],
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

impl <'state> State<'state> {
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self { lexer_state: lexer::State::new(str) }
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

    pub fn clone(&self) -> State<'state> {
        Self { lexer_state: self.lexer_state.clone() }
    }
}
