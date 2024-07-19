use crate::base::{ Program, TypeDeclaration, RunDeclaration, FunctionDeclaration, ConstructorDeclaration};
use crate::identifier::{Interner, Variable, ConstructorName, FunctionName, Identifier};
use crate::parser::lex::{
    lexer,
    lexer::{Request, LocatedToken, DeclarationKind},
    token::Keyword,
};
use crate::parser::program::pre_program;

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
    DuplicateVariableNames { duplicates: Vec<Identifier> },
    // Atleast one vector is non-empty.
    DuplicateNames {
        type_duplicates: Vec<Identifier>,
        constructor_duplicates: Vec<Identifier>,
        function_duplicates: Vec<Identifier>,
    },
    MoreThanOneRunDeclaration,
    RunDeclarationNotFound,
    FunctionHasDifferentNumberOfParametersThanDeclaredInItsType { declared_in_type: usize, parameters: usize },
}

#[derive(Debug)]
pub struct State<'lex_state, 'interner> {
    lexer_state: lexer::State<'lex_state>,
    interner: &'interner mut Interner,
}

pub fn parse_program(s: &str) -> Result<Program> {
    let mut program = Program::new();
    let mut state = State::new(s, program.interner_mut());
    let pre_program = pre_program(&mut state)?;

    for pre_decl in pre_program.type_declarations {
        let decl = TypeDeclaration::new(pre_decl,  &mut program.constructor_to_type_mapping);
        program.type_declarations_ordering.push(decl.name().clone());
        program.type_declarations.insert(decl.name().clone(), decl);
    }
    for decl in pre_program.function_declarations {
        program.function_declarations_ordering.push(decl.name());
        program.function_declarations.insert(decl.name(), decl);
    }
    program.run_declaration = pre_program.run_declarations.into_iter().nth(0);

    Ok(program)
}

pub enum Declaration {
    Type(PreTypeDeclaration),
    Run(RunDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Debug)]
pub struct PreProgram {
    pub type_declarations: Vec<PreTypeDeclaration>,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub run_declarations: Vec<RunDeclaration>,
}

#[derive(Debug)]
pub struct PreEnumDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub struct PreIndDeclaration {
    pub name: ConstructorName,
    pub type_parameters: Vec<Variable>,
    pub recursive_type_var: Variable,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub enum PreTypeDeclaration {
    Enum(PreEnumDeclaration),
    Ind(PreIndDeclaration),
}

impl PreProgram {
    pub fn new() -> Self {
        Self { type_declarations: vec![], function_declarations: vec![], run_declarations: vec![] }
    }

    pub fn add_declaration(&mut self, decl: Declaration) {
        match decl {
            Declaration::Type(type_declaration) => {
                self.type_declarations.push(type_declaration)
            },
            Declaration::Run(run_declaration) => {
                self.run_declarations.push(run_declaration)
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
