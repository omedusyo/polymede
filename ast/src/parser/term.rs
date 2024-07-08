use crate::parser::lex::{
    token::Keyword,
    lexer::Request,
};
use crate::parser::{
    base::{State, Result, Term},
    identifier::{Variable, variable},
    types::function_type_annotation,
    pattern::pattern_branches,
    program::function,
    special::{StartTerm, start_term, comma},
    combinator::{delimited_nonempty_sequence_to_vector, delimited_possibly_empty_sequence_to_vector},
};

pub fn term(state: &mut State) -> Result<Term> {
    match start_term(state)? {
        StartTerm::VariableUse(variable) => Ok(Term::VariableUse(variable)),
        StartTerm::FunctionApplication(function_name) => {
            state.request_token(Request::OpenParen)?;
            let args = possibly_empty_term_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::FunctionApplication(function_name, args))
        },
        StartTerm::ConstructorConstant(constructor_name) => Ok(Term::ConstructorUse(constructor_name, vec![])),
        StartTerm::ConstructorApplication(constructor_name) => {
            state.request_token(Request::OpenParen)?;
            let args = nonempty_term_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::ConstructorUse(constructor_name, args))
        },
        StartTerm::Match => {
            let arg = term(state)?;
            let branches = pattern_branches(state)?;
            Ok(Term::Match(Box::new(arg), branches))
        },
        StartTerm::Fold => {
            let arg = term(state)?;
            let branches = pattern_branches(state)?;
            Ok(Term::Match(Box::new(arg), branches))
        },
        StartTerm::Let => {
            state.request_token(Request::OpenCurly)?;
            let bindings = nonempty_var_binding_sequence(state)?;
            state.request_token(Request::BindingSeparator)?;
            let body = term(state)?;
            state.request_token(Request::CloseCurly)?;
            Ok(Term::Let(bindings, Box::new(body)))
        },
        StartTerm::Lambda => {
            let function_type = function_type_annotation(state)?;
            let function = function(state, function_type)?;
            Ok(Term::Lambda(Box::new(function)))
        },
        StartTerm::Apply => {
            let function = term(state)?;
            state.request_keyword(Keyword::To)?;
            state.request_token(Request::OpenParen)?;
            let args = possibly_empty_term_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::LambdaApplication(Box::new(function), args))
        },
    }
}

// Parses    x = expr
fn var_binding(state: &mut State) -> Result<(Variable, Term)> {
    let var = variable(state)?;
    state.request_keyword(Keyword::Eq)?;
    let term = term(state)?;
    Ok((var, term))
}

fn nonempty_var_binding_sequence(state: &mut State) -> Result<Vec<(Variable, Term)>> {
    state.consume_optional_comma()?;
    delimited_nonempty_sequence_to_vector(state, var_binding, comma)
}

fn possibly_empty_term_sequence(state: &mut State) -> Result<Vec<Term>> {
    delimited_possibly_empty_sequence_to_vector(state, term, comma)
}

fn nonempty_term_sequence(state: &mut State) -> Result<Vec<Term>> {
    delimited_nonempty_sequence_to_vector(state, term, comma)
}
