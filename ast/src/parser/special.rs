use crate::identifier::{Variable, ConstructorName};
use crate::parser::lex::{
    token::SeparatorSymbol,
    lexer::{Position, Request, LocatedToken},
};
use crate::parser::{
    base::{State, Result, Error},
    identifier::identifier,
};

pub fn comma(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } = state.request_token(Request::Separator(SeparatorSymbol::Comma))?;
    Ok(position)
}

pub fn or_separator(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } = state.request_token(Request::Separator(SeparatorSymbol::Or))?;
    Ok(position)
}

// TODO: Can we get rid of this?
pub fn do_nothing(state: &mut State) -> Result<()> {
    state.consume_whitespace_or_fail_when_end()
}

pub enum VariableOrConstructorName {
    Variable(Variable),
    ConstructorName(ConstructorName)
}

pub fn constructor_name_or_variable(state: &mut State) -> Result<VariableOrConstructorName> {
    let id = identifier(state)?;
    let c = id.first_char(state.interner());
    if c.is_ascii_uppercase() {
        // constructor
        Ok(VariableOrConstructorName::ConstructorName(id))
    } else if c.is_ascii_lowercase() {
        // variable
        Ok(VariableOrConstructorName::Variable(id))
    } else {
        Err(Error::ExpectedTypeConstructorOrTypeVar { received: id })
    }
}

pub enum StartTerm {
    TypeAnnotation,
    Int(i32),
    VariableUse(Variable),
    FunctionApplication(Variable),
    ConstructorConstant(ConstructorName),
    ConstructorApplication(ConstructorName),
    Match,
    Fold,
    Let,
    Apply,
    Lambda,
}

// TODO: This needs a refactor.
//       For type annotation it doesn't consume the `#` symbol,
//       but for fold/match/let/apply/fn it does consume the keyword (we're taking advantage of
//       that these keywords are valid identifiers)
pub fn start_term(state: &mut State) -> Result<StartTerm> {
    if state.is_next_token_start_type_annotation()? {
        return Ok(StartTerm::TypeAnnotation)
    } 

    match state.commit_if_next_token_int()? {
        Some(x) => return Ok(StartTerm::Int(x)),
        None => {},
    }

    let id = identifier(state)?;
    let c = id.first_char(state.interner());
    if c.is_ascii_uppercase() {
        // constructor
        if state.is_next_token_open_paren()? {
            Ok(StartTerm::ConstructorApplication(id))
        } else {
            // constructor constant
            Ok(StartTerm::ConstructorConstant(id))
        }
    } else if c.is_ascii_lowercase() {
        // TODO: Checking for fold/match here is really wrong. This should be the job for the lexer.
        match id.str(state.interner()) {
            "fold" => Ok(StartTerm::Fold),
            "match" => Ok(StartTerm::Match),
            "let" => Ok(StartTerm::Let),
            "apply" => Ok(StartTerm::Apply),
            "fn" => Ok(StartTerm::Lambda),
            _ => {
                if state.is_next_token_open_paren()? || state.is_next_token_open_angle()? {
                    Ok(StartTerm::FunctionApplication(id))
                } else {
                    Ok(StartTerm::VariableUse(id))
                }
            },
        }
    } else {
        Err(Error::ExpectedTerm { received: id })
    }
}
