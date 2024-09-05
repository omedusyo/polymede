use crate::base::{
    ConstructorDefinition, FunctionDefinition, Program, RunDefinition, Type, TypeDefinition,
};
use crate::identifier::{
    ConstructorName, FunctionName, Interner, RawIdentifier, TypeName, TypeVariable,
};
use crate::parser::lex::{
    lexer,
    lexer::{DefinitionKind, LocatedToken, Request},
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
    MoreThanOneRunDefinition,
    RunDefinitionNotFound,
    MsgTypeDefinitionNotFound,
    MoreThanOneMsgTypeDefinition,
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
    for pre_def in pre_program.type_defs {
        let def = TypeDefinition::new(pre_def, &mut program.constructor_to_type_mapping);
        program.type_definitions_ordering.push(def.name().clone());
        program.type_definitions.insert(def.name().clone(), def);
    }
    // ===msg type====
    program.msg_type = pre_program.msg_types.into_iter().next();

    // ===functions===
    for def in pre_program.function_definitions {
        program.function_definitions_ordering.push(def.name());
        program.function_definitions.insert(def.name(), def);
    }
    // ===run====
    program.run_definition = pre_program.run_definitions.into_iter().next();

    Ok(program)
}

pub enum Definition {
    Type(PreTypeDefinition),
    Run(RunDefinition),
    Function(FunctionDefinition),
    MsgType(PreTypeDefinition),
}

#[derive(Debug)]
pub struct PreProgram {
    pub type_defs: Vec<PreTypeDefinition>,
    pub function_definitions: Vec<FunctionDefinition>,
    pub run_definitions: Vec<RunDefinition>,
    pub msg_types: Vec<TypeName>,
}

#[derive(Debug)]
pub struct PreEnumDefinition {
    pub name: TypeName,
    pub type_parameters: Vec<TypeVariable>,
    pub constructors: Vec<ConstructorDefinition>,
}

#[derive(Debug)]
pub struct PreIndDefinition {
    pub name: TypeName,
    pub type_parameters: Vec<TypeVariable>,
    pub recursive_type_var: TypeVariable,
    pub constructors: Vec<ConstructorDefinition>,
}

#[derive(Debug)]
pub struct PreMsgTypeDefinition {
    pub type_definition: PreTypeDefinition,
}

#[derive(Debug)]
pub enum PreTypeDefinition {
    Enum(PreEnumDefinition),
    Ind(PreIndDefinition),
}

impl PreTypeDefinition {
    fn name(&self) -> &TypeName {
        match self {
            Self::Enum(def) => &def.name,
            Self::Ind(def) => &def.name,
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
            type_defs: vec![],
            function_definitions: vec![],
            run_definitions: vec![],
            msg_types: vec![],
        }
    }

    pub fn add_definition(&mut self, def: Definition) {
        use Definition::*;
        match def {
            Type(def) => self.type_defs.push(def),
            Run(def) => self.run_definitions.push(def),
            Function(def) => self.function_definitions.push(def),
            MsgType(def) => {
                self.msg_types.push(def.name().clone());
                self.type_defs.push(def);
            }
        }
    }

    pub fn type_names(&self) -> Vec<TypeName> {
        let mut names = vec![];
        for def in &self.type_defs {
            match def {
                PreTypeDefinition::Enum(def) => names.push(def.name.clone()),
                PreTypeDefinition::Ind(def) => names.push(def.name.clone()),
            }
        }
        names
    }

    pub fn constructor_names(&self) -> Vec<ConstructorName> {
        let mut names = vec![];
        for def in &self.type_defs {
            let constructors = match &def {
                PreTypeDefinition::Enum(def) => &def.constructors[..],
                PreTypeDefinition::Ind(def) => &def.constructors[..],
            };
            for constructor_def in constructors {
                names.push(constructor_def.name.clone())
            }
        }
        names
    }

    pub fn function_names(&self) -> Vec<FunctionName> {
        let mut names = vec![];
        for def in &self.function_definitions {
            names.push(def.name().clone())
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

    pub fn peek_definition_token(&mut self) -> Result<DefinitionKind> {
        self.lexer_state
            .peek_definition_token()
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
