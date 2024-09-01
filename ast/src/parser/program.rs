use crate::base::{
    ConstructorDeclaration, ForeignFunctionDeclaration, Function, FunctionDeclaration,
    FunctionType, RunDeclaration, TypedFunction, UserFunctionDeclaration,
};
use crate::identifier;
use crate::identifier::{FunctionName, Variable};
use crate::parser::lex::{
    lexer::{DeclarationKind, LocatedToken, Request},
    token::{Keyword, Token},
};
use crate::parser::{
    base::{
        Declaration, Error, PreEnumDeclaration, PreIndDeclaration, PreProgram, PreTypeDeclaration,
        Result, State,
    },
    combinator::delimited_possibly_empty_sequence_to_vector,
    identifier::{constructor_name, foreign_function_name, function_name, variable},
    pattern::{parameter_non_empty_sequence, parameter_possibly_empty_sequence},
    special::{do_nothing, or_separator},
    term::{term, typed_term},
    types::{foreign_function_type, function_type_annotation, type_nonempty_sequence},
};

pub fn pre_program(state: &mut State) -> Result<PreProgram> {
    let mut program = PreProgram::new();

    for declaration in
        delimited_possibly_empty_sequence_to_vector(state, program_declaration, do_nothing)?
    {
        program.add_declaration(declaration);
    }
    check_program_names_uniqueness(&program)?;
    Ok(program)
}

fn check_program_names_uniqueness(program: &PreProgram) -> Result<()> {
    let type_duplicates = identifier::duplicates(&program.type_names());
    let constructor_duplicates = identifier::duplicates(&program.constructor_names());
    let function_duplicates = identifier::duplicates(&program.function_names());
    if !(type_duplicates.is_empty()
        && constructor_duplicates.is_empty()
        && function_duplicates.is_empty())
    {
        return Err(Error::DuplicateNames {
            type_duplicates,
            constructor_duplicates,
            function_duplicates,
        });
    }

    match program.run_declarations.len() {
        0 => return Err(Error::RunDeclarationNotFound),
        1 => {}
        _ => return Err(Error::MoreThanOneRunDeclaration),
    }

    match program.msg_types.len() {
        0 => return Err(Error::MsgTypeDeclarationNotFound),
        1 => {}
        _ => return Err(Error::MoreThanOneMsgTypeDeclaration),
    }

    Ok(())
}

fn program_declaration(state: &mut State) -> Result<Declaration> {
    use DeclarationKind::*;
    match state.peek_declaration_token()? {
        Type => Ok(Declaration::Type(type_declaration(state)?)),
        Run => Ok(Declaration::Run(run_declaration(state)?)),
        UserFunction => Ok(Declaration::Function(FunctionDeclaration::User(
            user_function_declaration(state)?,
        ))),
        ForeignFunction => Ok(Declaration::Function(FunctionDeclaration::Foreign(
            foreign_function_declaration(state)?,
        ))),
        MsgType => Ok(Declaration::MsgType(msg_type_declaration(state)?)),
    }
}

fn constructor_declaration(state: &mut State) -> Result<ConstructorDeclaration> {
    let constructor_name = constructor_name(state)?;

    if state.is_next_token_open_paren()? {
        // Constructor with non-zero parameters
        state.request_token(Request::OpenParen)?;
        let parameter_types = type_nonempty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        Ok(ConstructorDeclaration {
            name: constructor_name,
            parameters: parameter_types,
        })
    } else {
        // Constant
        Ok(ConstructorDeclaration {
            name: constructor_name,
            parameters: vec![],
        })
    }
}

fn constructor_declaration_sequence(state: &mut State) -> Result<Vec<ConstructorDeclaration>> {
    state.consume_optional_or()?;
    delimited_possibly_empty_sequence_to_vector(state, constructor_declaration, or_separator)
}

static FORBIDDEN_TYPE_NAMES: [&str; 5] = ["Fn", "I32", "F32", "String", "Cmd"];

fn is_type_name_forbidden(name: &str) -> bool {
    for forbidden_name in FORBIDDEN_TYPE_NAMES {
        if forbidden_name == name {
            return true;
        }
    }
    false
}

// TODO: Make sure that for ind type declarations the recursive type-variable doesn't shadow any of
// its parameters.
pub fn type_declaration(state: &mut State) -> Result<PreTypeDeclaration> {
    state.request_keyword(Keyword::Type_)?;

    let type_name = constructor_name(state)?;

    {
        let constructor_name_str = type_name.str(state.interner());
        if is_type_name_forbidden(constructor_name_str) {
            return Err(Error::TypeHasForbiddenName {
                received: constructor_name_str.to_string(),
            });
        }
    }

    let type_parameters: Vec<Variable> = if state.is_next_token_open_paren()? {
        state.request_token(Request::OpenParen)?;
        let params = parameter_non_empty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        params
    } else {
        vec![]
    };

    state.request_keyword(Keyword::Eq)?;

    let LocatedToken {
        token: Token::Keyword(keyword),
        ..
    } = state.request_token(Request::TypeDeclarationKeyword)?
    else {
        unreachable!()
    };
    state.request_token(Request::OpenCurly)?;
    let declaration = match keyword {
        Keyword::Enum => PreTypeDeclaration::Enum(PreEnumDeclaration {
            name: type_name,
            type_parameters,
            constructors: constructor_declaration_sequence(state)?,
        }),
        Keyword::Ind => {
            let recursive_type_var = variable(state)?;

            state.request_token(Request::BindingSeparator)?;

            PreTypeDeclaration::Ind(PreIndDeclaration {
                name: type_name,
                type_parameters,
                recursive_type_var,
                constructors: constructor_declaration_sequence(state)?,
            })
        }
        _ => unreachable!(),
    };
    state.request_token(Request::CloseCurly)?;

    Ok(declaration)
}

pub fn msg_type_declaration(state: &mut State) -> Result<PreTypeDeclaration> {
    state.request_keyword(Keyword::Msg)?;
    // TODO: It seems that this will allow `msgtype`.
    let type_declaration = type_declaration(state)?;
    Ok(type_declaration)
}

// ===Function Declarations===
pub fn foreign_function_declaration(state: &mut State) -> Result<ForeignFunctionDeclaration> {
    state.request_keyword(Keyword::Foreign)?;
    let external_name = foreign_function_name(state)?;
    state.request_keyword(Keyword::Fn)?;
    let name = function_name(state)?;
    state.request_keyword(Keyword::TypeAnnotationSeparator)?;
    let type_ = foreign_function_type(state)?;
    Ok(ForeignFunctionDeclaration {
        name,
        type_,
        external_name,
    })
}

pub fn user_function_declaration(state: &mut State) -> Result<UserFunctionDeclaration> {
    state.request_keyword(Keyword::Fn)?;
    let function_name = function_name(state)?;
    state.request_keyword(Keyword::Eq)?;

    fn inner_function_declaration(
        state: &mut State,
        function_name: FunctionName,
        type_parameters: Vec<Variable>,
    ) -> Result<UserFunctionDeclaration> {
        let type_ = function_type_annotation(state)?;
        let function = typed_function(state, type_)?;
        Ok(UserFunctionDeclaration {
            name: function_name,
            type_parameters,
            function,
        })
    }

    if state.commit_if_next_token_forall()? {
        // forall encountered
        state.request_token(Request::OpenCurly)?;
        let type_parameters = parameter_non_empty_sequence(state)?;
        state.request_token(Request::BindingSeparator)?;

        let declaration = inner_function_declaration(state, function_name, type_parameters)?;

        state.request_token(Request::CloseCurly)?;
        Ok(declaration)
    } else {
        // no forall
        inner_function_declaration(state, function_name, vec![])
    }
}

pub fn function(state: &mut State) -> Result<Function> {
    state.request_token(Request::OpenCurly)?;

    let parameters = parameter_possibly_empty_sequence(state)?;
    state.request_token(Request::BindingSeparator)?;
    let body = term(state)?;

    state.request_token(Request::CloseCurly)?;

    Ok(Function { parameters, body })
}

pub fn typed_function(state: &mut State, type_: FunctionType) -> Result<TypedFunction> {
    let function = function(state)?;

    if type_.input_types.len() != function.parameters.len() {
        Err(
            Error::FunctionHasDifferentNumberOfParametersThanDeclaredInItsType {
                declared_in_type: type_.input_types.len(),
                parameters: function.parameters.len(),
            },
        )
    } else {
        Ok(TypedFunction { type_, function })
    }
}

pub fn run_declaration(state: &mut State) -> Result<RunDeclaration> {
    state.request_keyword(Keyword::Run)?;

    let typed_term = typed_term(state)?;
    Ok(RunDeclaration { body: typed_term })
}
