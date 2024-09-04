use crate::base::{
    ConstructorDeclaration, FunctionDeclaration, Program, RunDeclaration, Type, TypeDeclaration,
};
use crate::identifier::{
    ConstructorName, FunctionName, Interner, RawIdentifier, TypeName, TypeVariable,
};
use crate::parser::lex::{
    lexer,
    lexer::{DeclarationKind, LocatedToken, Request},
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
    Lex(lexer::ErrorWithPosition),
    ExpectedTypeConstructorOrTypeVar {
        received: RawIdentifier,
    },
    ExpectedTypeConstructorOrTypeVarOrAnythingInPattern {
        received: RawIdentifier,
    },
    ExpectedTerm {
        received: RawIdentifier,
    },
    ExpectedVariable {
        received: RawIdentifier,
    },
    ExpectedPrimitiveType {
        received: Type,
    },
    DuplicateVariableNames {
        duplicates: Vec<RawIdentifier>,
    },
    ExpectedEqualsOrAssignmentSymbol,
    // Atleast one vector is non-empty.
    DuplicateNames {
        type_duplicates: Vec<TypeName>,
        constructor_duplicates: Vec<ConstructorName>,
        function_duplicates: Vec<FunctionName>,
    },
    TypeHasForbiddenName {
        received: String,
    },
    MoreThanOneRunDeclaration,
    RunDeclarationNotFound,
    MsgTypeDeclarationNotFound,
    MoreThanOneMsgTypeDeclaration,
    FunctionHasDifferentNumberOfParametersThanDeclaredInItsType {
        declared_in_type: usize,
        parameters: usize,
    },
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

    // ===types===
    for pre_decl in pre_program.type_declarations {
        let decl = TypeDeclaration::new(pre_decl, &mut program.constructor_to_type_mapping);
        program.type_declarations_ordering.push(decl.name().clone());
        program.type_declarations.insert(decl.name().clone(), decl);
    }
    // ===msg type====
    program.msg_type = pre_program.msg_types.into_iter().next();

    // ===functions===
    for decl in pre_program.function_declarations {
        program.function_declarations_ordering.push(decl.name());
        program.function_declarations.insert(decl.name(), decl);
    }
    // ===run====
    program.run_declaration = pre_program.run_declarations.into_iter().next();

    Ok(program)
}

pub enum Declaration {
    Type(PreTypeDeclaration),
    Run(RunDeclaration),
    Function(FunctionDeclaration),
    MsgType(PreTypeDeclaration),
}

#[derive(Debug)]
pub struct PreProgram {
    pub type_declarations: Vec<PreTypeDeclaration>,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub run_declarations: Vec<RunDeclaration>,
    pub msg_types: Vec<TypeName>,
}

#[derive(Debug)]
pub struct PreEnumDeclaration {
    pub name: TypeName,
    pub type_parameters: Vec<TypeVariable>,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub struct PreIndDeclaration {
    pub name: TypeName,
    pub type_parameters: Vec<TypeVariable>,
    pub recursive_type_var: TypeVariable,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
pub struct PreMsgTypeDeclaration {
    pub type_declaration: PreTypeDeclaration,
}

#[derive(Debug)]
pub enum PreTypeDeclaration {
    Enum(PreEnumDeclaration),
    Ind(PreIndDeclaration),
}

impl PreTypeDeclaration {
    fn name(&self) -> &TypeName {
        match self {
            Self::Enum(decl) => &decl.name,
            Self::Ind(decl) => &decl.name,
        }
    }
}

impl Default for PreProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl PreProgram {
    pub fn new() -> Self {
        Self {
            type_declarations: vec![],
            function_declarations: vec![],
            run_declarations: vec![],
            msg_types: vec![],
        }
    }

    pub fn add_declaration(&mut self, decl: Declaration) {
        use Declaration::*;
        match decl {
            Type(type_declaration) => self.type_declarations.push(type_declaration),
            Run(run_declaration) => self.run_declarations.push(run_declaration),
            Function(function_declaration) => self.function_declarations.push(function_declaration),
            MsgType(type_declaration) => {
                self.msg_types.push(type_declaration.name().clone());
                self.type_declarations.push(type_declaration);
            }
        }
    }

    pub fn type_names(&self) -> Vec<TypeName> {
        let mut names = vec![];
        for declaration in &self.type_declarations {
            match declaration {
                PreTypeDeclaration::Enum(declaration) => names.push(declaration.name.clone()),
                PreTypeDeclaration::Ind(declaration) => names.push(declaration.name.clone()),
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
            names.push(declaration.name().clone())
        }
        names
    }
}

impl<'lex_state, 'interner> State<'lex_state, 'interner> {
    pub fn new<'a: 'lex_state>(str: &'a str, interner: &'interner mut Interner) -> Self {
        Self {
            interner,
            lexer_state: lexer::State::new(str),
        }
    }

    pub fn interner(&mut self) -> &mut Interner {
        self.interner
    }

    pub fn request_token(&mut self, request: Request) -> Result<LocatedToken> {
        self.lexer_state.request(request).map_err(Error::Lex)
    }

    pub fn request_keyword(&mut self, keyword: Keyword) -> Result<LocatedToken> {
        self.request_token(Request::Keyword(keyword))
    }

    pub fn consume_optional_or(&mut self) -> Result<()> {
        self.lexer_state.consume_optional_or().map_err(Error::Lex)
    }

    pub fn consume_whitespace_or_fail_when_end(&mut self) -> Result<()> {
        self.lexer_state
            .consume_whitespace_or_fail_when_end()
            .map_err(Error::Lex)
    }

    pub fn consume_optional_comma(&mut self) -> Result<()> {
        self.lexer_state
            .consume_optional_comma()
            .map_err(Error::Lex)
    }

    pub fn is_next_token_open_paren(&mut self) -> Result<bool> {
        self.lexer_state
            .is_next_token_open_paren()
            .map_err(Error::Lex)
    }

    pub fn is_next_token_open_angle(&mut self) -> Result<bool> {
        self.lexer_state
            .is_next_token_open_angle()
            .map_err(Error::Lex)
    }

    pub fn is_next_token_eq(&mut self) -> Result<bool> {
        self.lexer_state.is_next_token_eq().map_err(Error::Lex)
    }

    pub fn is_next_token_start_type_annotation(&mut self) -> Result<bool> {
        self.lexer_state
            .is_next_token_start_type_annotation()
            .map_err(Error::Lex)
    }

    pub fn commit_if_next_token_int(&mut self) -> Result<Option<i32>> {
        self.lexer_state
            .commit_if_next_token_int()
            .map_err(Error::Lex)
    }

    pub fn commit_if_next_token_f32(&mut self) -> Result<Option<f32>> {
        self.lexer_state
            .commit_if_next_token_f32()
            .map_err(Error::Lex)
    }

    pub fn commit_if_next_token_string_literal(&mut self) -> Result<Option<String>> {
        self.lexer_state
            .commit_if_next_token_string_literal()
            .map_err(Error::Lex)
    }

    pub fn commit_if_next_token_forall(&mut self) -> Result<bool> {
        self.lexer_state
            .commit_if_next_token_forall()
            .map_err(Error::Lex)
    }

    pub fn peek_declaration_token(&mut self) -> Result<DeclarationKind> {
        self.lexer_state
            .peek_declaration_token()
            .map_err(Error::Lex)
    }

    pub fn save_copy(&self) -> lexer::State<'lex_state> {
        self.lexer_state.save_copy()
    }

    pub fn read_char_or_fail_when_end(&mut self) -> Result<char> {
        self.lexer_state
            .read_char_or_fail_when_end()
            .map_err(Error::Lex)
    }

    pub fn consume_char_or_fail_when_end(&mut self) -> Result<char> {
        self.lexer_state
            .consume_char_or_fail_when_end()
            .map_err(Error::Lex)
    }

    pub fn restore(&mut self, lexer_state: lexer::State<'lex_state>) {
        self.lexer_state = lexer_state;
    }
}
