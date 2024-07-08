use crate::parser::lex::{
    token::Keyword,
    lexer::Request,
};
use crate::parser::{
    base::{State, Result, FunctionType, Type},
    special::{VariableOrConstructorName, comma, constructor_name_or_variable},
    combinator::{delimited_nonempty_sequence_to_vector, delimited_possibly_empty_sequence_to_vector},
};

// Parses
//   x
//   Cons(T1, T2)
//   Fn(T1, T2 -> T)
pub fn type_(state: &mut State) -> Result<Type> {
    match constructor_name_or_variable(state)? {
        VariableOrConstructorName::Variable(variable) => Ok(Type::VariableUse(variable)),
        VariableOrConstructorName::ConstructorName(constructor_name) => {
            if state.is_next_token_open_paren()? {
                // A type constructor with multiple parameters.
                state.request_token(Request::OpenParen)?;
                if constructor_name.str() == "Fn" {
                    let fn_type = function_type(state)?;
                    state.request_token(Request::CloseParen)?;
                    Ok(Type::Arrow(Box::new(fn_type)))
                } else {
                    let type_args = type_nonempty_sequence(state)?;
                    state.request_token(Request::CloseParen)?;
                    Ok(Type::TypeApplication(constructor_name, type_args))
                }
            } else {
                // A type constant
                Ok(Type::TypeApplication(constructor_name, vec![]))
            }
        }
    }
}

// Parses   T1, T2, T3, T4 possibly empty
pub fn type_possibly_empty_sequence(state: &mut State) -> Result<Vec<Type>> {
    delimited_possibly_empty_sequence_to_vector(state, type_, comma)
}

pub fn type_nonempty_sequence(state: &mut State) -> Result<Vec<Type>> {
    delimited_nonempty_sequence_to_vector(state, type_, comma)
}

// Parses  T1, T2 -> T
pub fn function_type(state: &mut State) -> Result<FunctionType> {
    let input_types = type_possibly_empty_sequence(state)?;
    state.request_keyword(Keyword::Arrow)?;
    let output_type = type_(state)?;
    Ok(FunctionType { input_types, output_type })
}

// Parses
//    # type :
// where `:` can be optional when newline is present.
// TODO: Consider having the `:` completely optional for function declarations.
pub fn function_type_annotation(state: &mut State) -> Result<FunctionType> {
    state.request_keyword(Keyword::TypeAnnotationStart)?;
    let type_ = function_type(state)?;
    // TODO: Consume whitespace that's not a newline until you reach a newline or ':'
    state.request_keyword(Keyword::TypeAnnotationSeparator)?;
    Ok(type_)
}

pub fn type_annotation(state: &mut State) -> Result<Type> {
    state.request_keyword(Keyword::TypeAnnotationStart)?;
    let type_ = type_(state)?;
    // TODO: Consume whitespace that's not a newline until you reach a newline or ':'
    state.request_keyword(Keyword::TypeAnnotationSeparator)?;
    Ok(type_)
}