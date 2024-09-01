use crate::base::{DoBinding, Term, Type, TypedTerm};
use crate::identifier::Variable;
use crate::parser::lex::{lexer::Request, token::Keyword};
use crate::parser::{
    base::{Error, Result, State},
    combinator::{
        delimited_nonempty_sequence_to_vector, delimited_possibly_empty_sequence_to_vector,
    },
    identifier::variable,
    pattern::pattern_branches,
    program::function,
    special::{comma, start_term, StartTerm},
    types::{type_annotation, type_nonempty_sequence},
};

pub fn term(state: &mut State) -> Result<Term> {
    match start_term(state)? {
        StartTerm::TypeAnnotation => {
            let type_ = type_annotation(state)?;
            let term = term(state)?;
            Ok(Term::TypedTerm(Box::new(TypedTerm { type_, term })))
        }
        StartTerm::Int(x) => Ok(Term::Int(x)),
        StartTerm::Float(x) => Ok(Term::Float(x)),
        StartTerm::StringLiteral(s) => Ok(Term::StringLiteral(s)),
        StartTerm::VariableUse(variable) => Ok(Term::VariableUse(variable)),
        StartTerm::FunctionApplication(function_name) => {
            let type_args: Vec<Type> = if state.is_next_token_open_angle()? {
                state.request_token(Request::OpenAngle)?;
                let type_args = type_nonempty_sequence(state)?;
                state.request_token(Request::CloseAngle)?;
                type_args
            } else {
                vec![]
            };
            state.request_token(Request::OpenParen)?;
            let args = possibly_empty_term_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::FunctionApplication(function_name, type_args, args))
        }
        StartTerm::ConstructorConstant(constructor_name) => {
            Ok(Term::ConstructorUse(constructor_name, vec![]))
        }
        StartTerm::ConstructorApplication(constructor_name) => {
            state.request_token(Request::OpenParen)?;
            let args = nonempty_term_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::ConstructorUse(constructor_name, args))
        }
        StartTerm::Match => {
            let arg = term(state)?;
            let branches = pattern_branches(state)?;
            Ok(Term::Match(Box::new(arg), branches))
        }
        StartTerm::Fold => {
            let arg = term(state)?;
            let branches = pattern_branches(state)?;
            Ok(Term::Fold(Box::new(arg), branches))
        }
        StartTerm::Let => {
            state.request_token(Request::OpenCurly)?;
            let bindings = nonempty_var_binding_sequence(state)?;
            state.request_token(Request::BindingSeparator)?;
            let body = term(state)?;
            state.request_token(Request::CloseCurly)?;
            Ok(Term::Let(bindings, Box::new(body)))
        }
        StartTerm::Lambda => {
            let function = function(state)?;
            Ok(Term::Lambda(Box::new(function)))
        }
        StartTerm::Apply => {
            let function = term(state)?;
            state.request_keyword(Keyword::To)?;
            state.request_token(Request::OpenParen)?;
            let args = possibly_empty_term_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::LambdaApplication(Box::new(function), args))
        }
        StartTerm::Pure => {
            state.request_token(Request::OpenParen)?;
            let arg = term(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::Pure(Box::new(arg)))
        }
        StartTerm::Do => {
            state.request_token(Request::OpenCurly)?;
            let bindings = nonempty_do_binding_sequence(state)?;
            state.request_token(Request::BindingSeparator)?;
            let body = term(state)?;
            state.request_token(Request::CloseCurly)?;
            Ok(Term::Do(bindings, Box::new(body)))
        }
        StartTerm::Receive => Ok(Term::Receive),
    }
}

pub fn typed_term(state: &mut State) -> Result<TypedTerm> {
    let type_ = type_annotation(state)?;
    let term = term(state)?;
    Ok(TypedTerm { type_, term })
}

// Parses    x = expr
fn var_binding(state: &mut State) -> Result<(Variable, Term)> {
    let var = variable(state)?;
    state.request_keyword(Keyword::Eq)?;
    let term = term(state)?;
    Ok((var, term))
}

fn do_binding(state: &mut State) -> Result<DoBinding> {
    let var = variable(state)?;
    if state.is_next_token_open_angle()? {
        state.request_keyword(Keyword::Assign)?;
        let term = term(state)?;
        Ok(DoBinding::ExecuteThenBind(var, term))
    } else if state.is_next_token_eq()? {
        state.request_keyword(Keyword::Eq)?;
        let term = term(state)?;
        Ok(DoBinding::Bind(var, term))
    } else {
        Err(Error::ExpectedEqualsOrAssignmentSymbol)
    }
}

fn nonempty_var_binding_sequence(state: &mut State) -> Result<Vec<(Variable, Term)>> {
    state.consume_optional_comma()?;
    delimited_nonempty_sequence_to_vector(state, var_binding, comma)
}

fn nonempty_do_binding_sequence(state: &mut State) -> Result<Vec<DoBinding>> {
    state.consume_optional_comma()?;
    delimited_nonempty_sequence_to_vector(state, do_binding, comma)
}

fn possibly_empty_term_sequence(state: &mut State) -> Result<Vec<Term>> {
    delimited_possibly_empty_sequence_to_vector(state, term, comma)
}

fn nonempty_term_sequence(state: &mut State) -> Result<Vec<Term>> {
    delimited_nonempty_sequence_to_vector(state, term, comma)
}
