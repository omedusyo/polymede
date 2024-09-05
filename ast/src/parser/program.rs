use crate::base::{
    ConstructorDefinition, ForeignFunctionBinding, Function, FunctionDefinition,
    FunctionType, RunDefinition, TypedFunction, UserFunctionBinding,
};
use crate::identifier;
use crate::identifier::{FunctionName, Identifier, TypeVariable};
use crate::parser::lex::{
    lexer::{DefinitionKind, LocatedToken, Request},
    token::{Keyword, Token},
};
use crate::parser::{
    base::{
        Definition, Error, PreEnumDefinition, PreIndDefinition, PreProgram, PreTypeDefinition,
        Result, State,
    },
    combinator::delimited_possibly_empty_sequence_to_vector,
    identifier::{
        constructor_name, foreign_function_name, function_name, parameter_non_empty_sequence,
        parameter_possibly_empty_sequence, type_name, type_variable,
    },
    special::{do_nothing, or_separator},
    term::{term, typed_term},
    types::{foreign_function_type, function_type_annotation, type_nonempty_sequence},
};

pub fn pre_program(state: &mut State) -> Result<PreProgram> {
    let mut program = PreProgram::new();

    for definition in
        delimited_possibly_empty_sequence_to_vector(state, program_definition, do_nothing)?
    {
        program.add_definition(definition);
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

    match program.run_definitions.len() {
        0 => return Err(Error::RunDefinitionNotFound),
        1 => {}
        _ => return Err(Error::MoreThanOneRunDefinition),
    }

    match program.msg_types.len() {
        0 => return Err(Error::MsgTypeDefinitionNotFound),
        1 => {}
        _ => return Err(Error::MoreThanOneMsgTypeDefinition),
    }

    Ok(())
}

fn program_definition(state: &mut State) -> Result<Definition> {
    use DefinitionKind::*;
    match state.peek_definition_token()? {
        Type => Ok(Definition::Type(type_definition(state)?)),
        Run => Ok(Definition::Run(run_definition(state)?)),
        UserFunction => Ok(Definition::Function(FunctionDefinition::User(
            user_function_definition(state)?,
        ))),
        ForeignFunction => Ok(Definition::Function(FunctionDefinition::Foreign(
            foreign_function_definition(state)?,
        ))),
        MsgType => Ok(Definition::MsgType(msg_type_definition(state)?)),
    }
}

fn constructor_definition(state: &mut State) -> Result<ConstructorDefinition> {
    let constructor_name = constructor_name(state)?;

    if state.is_next_token_open_paren()? {
        // Constructor with non-zero parameters
        state.request_token(Request::OpenParen)?;
        let parameter_types = type_nonempty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        Ok(ConstructorDefinition {
            name: constructor_name,
            parameters: parameter_types,
        })
    } else {
        // Constant
        Ok(ConstructorDefinition {
            name: constructor_name,
            parameters: vec![],
        })
    }
}

fn constructor_definition_sequence(state: &mut State) -> Result<Vec<ConstructorDefinition>> {
    state.consume_optional_or()?;
    delimited_possibly_empty_sequence_to_vector(state, constructor_definition, or_separator)
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

// TODO: Make sure that for ind type definitions the recursive type-variable doesn't shadow any of
// its parameters.
pub fn type_definition(state: &mut State) -> Result<PreTypeDefinition> {
    state.request_keyword(Keyword::Type_)?;

    let type_name = type_name(state)?;

    {
        let constructor_name_str = type_name.str(state.interner());
        if is_type_name_forbidden(constructor_name_str) {
            return Err(Error::TypeHasForbiddenName {
                received: constructor_name_str.to_string(),
            });
        }
    }

    let type_parameters: Vec<TypeVariable> = if state.is_next_token_open_paren()? {
        state.request_token(Request::OpenParen)?;
        let params = parameter_non_empty_sequence(state)?
            .into_iter()
            .map(TypeVariable)
            .collect();
        state.request_token(Request::CloseParen)?;
        params
    } else {
        vec![]
    };

    state.request_keyword(Keyword::Eq)?;

    let LocatedToken {
        token: Token::Keyword(keyword),
        ..
    } = state.request_token(Request::TypeDefinitionKeyword)?
    else {
        unreachable!()
    };
    state.request_token(Request::OpenCurly)?;
    let definition = match keyword {
        Keyword::Enum => PreTypeDefinition::Enum(PreEnumDefinition {
            name: type_name,
            type_parameters,
            constructors: constructor_definition_sequence(state)?,
        }),
        Keyword::Ind => {
            let recursive_type_var = type_variable(state)?;

            state.request_token(Request::BindingSeparator)?;

            PreTypeDefinition::Ind(PreIndDefinition {
                name: type_name,
                type_parameters,
                recursive_type_var,
                constructors: constructor_definition_sequence(state)?,
            })
        }
        _ => unreachable!(),
    };
    state.request_token(Request::CloseCurly)?;

    Ok(definition)
}

pub fn msg_type_definition(state: &mut State) -> Result<PreTypeDefinition> {
    state.request_keyword(Keyword::Msg)?;
    // TODO: It seems that this will allow `msgtype`.
    let type_definition = type_definition(state)?;
    Ok(type_definition)
}

// ===Function Definitions===
pub fn foreign_function_definition(state: &mut State) -> Result<ForeignFunctionBinding> {
    state.request_keyword(Keyword::Foreign)?;
    let external_name = foreign_function_name(state)?;
    state.request_keyword(Keyword::Fn)?;
    let name = function_name(state)?;
    state.request_keyword(Keyword::TypeAnnotationSeparator)?;
    let type_ = foreign_function_type(state)?;
    Ok(ForeignFunctionBinding {
        name,
        type_,
        external_name,
    })
}

pub fn user_function_definition(state: &mut State) -> Result<UserFunctionBinding> {
    state.request_keyword(Keyword::Fn)?;
    let function_name = function_name(state)?;
    state.request_keyword(Keyword::Eq)?;

    fn inner_function_definition(
        state: &mut State,
        function_name: FunctionName,
        type_parameters: Vec<TypeVariable>,
    ) -> Result<UserFunctionBinding> {
        let type_ = function_type_annotation(state)?;
        let function = typed_function(state, type_)?;
        Ok(UserFunctionBinding {
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

        let definition = inner_function_definition(state, function_name, type_parameters)?;

        state.request_token(Request::CloseCurly)?;
        Ok(definition)
    } else {
        // no forall
        inner_function_definition(state, function_name, vec![])
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

pub fn run_definition(state: &mut State) -> Result<RunDefinition> {
    state.request_keyword(Keyword::Run)?;

    let typed_term = typed_term(state)?;
    Ok(RunDefinition { body: typed_term })
}
