use crate::parser::lex::{
    token::{Token, Keyword},
    lexer::{Request, LocatedToken, DeclarationKind},
};
use crate::parser::{
    base::{State, Result, Error, PreProgram, Declaration, LetDeclaration, Function, FunctionType, FunctionDeclaration, IndDeclaration, EnumDeclaration, TypeDeclaration, ConstructorDeclaration},
    identifier,
    identifier::{Variable, FunctionName, variable, constructor_name, function_name},
    term::{term, typed_term},
    types::{type_nonempty_sequence, function_type_annotation},
    pattern::{parameter_non_empty_sequence, parameter_possibly_empty_sequence},
    special::{or_separator, do_nothing},
    combinator::delimited_possibly_empty_sequence_to_vector,
};
use std::collections::HashMap;

pub fn pre_program(state: &mut State) -> Result<PreProgram> {
    let mut program = PreProgram::new();

    for declaration in delimited_possibly_empty_sequence_to_vector(state, program_declaration, do_nothing)? {
        program.add_declaration(declaration);
    }
    check_program_names_uniqueness(&program)?;
    Ok(program)
}

fn check_program_names_uniqueness(program: &PreProgram) -> Result<()> {
    let type_duplicates = identifier::duplicates(&program.type_names());
    let constructor_duplicates = identifier::duplicates(&program.constructor_names());
    let function_duplicates = identifier::duplicates(&program.function_names());
    let let_duplicates = identifier::duplicates(&program.let_names());
    if !(type_duplicates.is_empty() && constructor_duplicates.is_empty() && function_duplicates.is_empty() && let_duplicates.is_empty()) {
        Err(Error::DuplicateNames {
            type_duplicates,
            constructor_duplicates,
            function_duplicates,
            let_duplicates,
        })
    } else {
        Ok(())
    }
}

fn program_declaration(state: &mut State) -> Result<Declaration> {
    match state.peek_declaration_token()? {
        DeclarationKind::Type => Ok(Declaration::Type(type_declaration(state)?)),
        DeclarationKind::Let => Ok(Declaration::Let(let_declaration(state)?)),
        DeclarationKind::Function => Ok(Declaration::Function(function_declaration(state)?)),
    }
}

fn constructor_declaration(state: &mut State) -> Result<ConstructorDeclaration> {
    let constructor_name = constructor_name(state)?;

    if state.is_next_token_open_paren()? {
        // Constructor with non-zero parameters
        state.request_token(Request::OpenParen)?;
        let parameter_types = type_nonempty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        Ok(ConstructorDeclaration { name: constructor_name, parameters: parameter_types, })
    } else {
        // Constant
        Ok(ConstructorDeclaration { name: constructor_name, parameters: vec![] })
    }
}

fn constructor_declaration_sequence(state: &mut State) -> Result<Vec<ConstructorDeclaration>> {
    state.consume_optional_or()?;
    delimited_possibly_empty_sequence_to_vector( state, constructor_declaration, or_separator)
}

pub fn type_declaration(state: &mut State) -> Result<TypeDeclaration> {
    state.request_keyword(Keyword::Type_)?;

    let constructor_name = constructor_name(state)?;

    let type_parameters: Vec<Variable> = if state.is_next_token_open_paren()? {
        state.request_token(Request::OpenParen)?;
        let params = parameter_non_empty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        params
    } else {
        vec![]
    };

    state.request_keyword(Keyword::Eq)?;

    let LocatedToken { token: Token::Keyword(keyword), .. } = state.request_token(Request::TypeDeclarationKeyword)? else { unreachable!() };
    state.request_token(Request::OpenCurly)?;
    let declaration = match keyword {
        Keyword::Enum => {
            // TODO
            //let constructors = HashMap::new();
            TypeDeclaration::Enum(EnumDeclaration {
                name: constructor_name,
                type_parameters,
                constructors: constructor_declaration_sequence(state)?
            })
        },
        Keyword::Ind => {
            let recursive_type_var = variable(state)?;

            state.request_token(Request::BindingSeparator)?;

            TypeDeclaration::Ind(IndDeclaration {
                name: constructor_name,
                type_parameters,
                recursive_type_var,
                constructors: constructor_declaration_sequence(state)?
            })
        },
        _ => unreachable!(),
    };
    state.request_token(Request::CloseCurly)?;

    Ok(declaration)
}

// ===Function/Let Declarations===
pub fn function_declaration(state: &mut State) -> Result<FunctionDeclaration> {
    state.request_keyword(Keyword::Fn)?;
    let function_name = function_name(state)?;
    state.request_keyword(Keyword::Eq)?;


    fn inner_function_declaration(state: &mut State, function_name: FunctionName, type_parameters: Vec<Variable>) -> Result<FunctionDeclaration> {
        let type_ = function_type_annotation(state)?;
        let function = function(state, type_)?;
        Ok(FunctionDeclaration { name: function_name, type_parameters, function })
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

pub fn function(state: &mut State, type_: FunctionType) -> Result<Function> {
    state.request_token(Request::OpenCurly)?;

    let parameters = parameter_possibly_empty_sequence(state)?;
    state.request_token(Request::BindingSeparator)?;
    let body = term(state)?;

    state.request_token(Request::CloseCurly)?;

    if type_.input_types.len() != parameters.len() {
        Err(Error::FunctionHasDifferentNumberOfParametersThanDeclaredInItsType { declared_in_type: type_.input_types.len(), parameters: parameters.len() })
    } else {
        Ok(Function { type_, parameters, body })
    }
}

pub fn let_declaration(state: &mut State) -> Result<LetDeclaration> {
    state.request_keyword(Keyword::Let)?;
    let name = function_name(state)?;
    state.request_keyword(Keyword::Eq)?;


    fn inner_let_declaration(state: &mut State, function_name: FunctionName, type_parameters: Vec<Variable>) -> Result<LetDeclaration> {
        let typed_term = typed_term(state)?;
        Ok(LetDeclaration { name: function_name, type_parameters, body: typed_term })
    }
    
    if state.commit_if_next_token_forall()? {
        // forall encountered
        state.request_token(Request::OpenCurly)?;
        let type_parameters = parameter_non_empty_sequence(state)?;
        state.request_token(Request::BindingSeparator)?;

        let declaration = inner_let_declaration(state, name, type_parameters)?;

        state.request_token(Request::CloseCurly)?;
        Ok(declaration)
    } else {
        // no forall
        inner_let_declaration(state, name, vec![])
    }
}
