use crate::identifier::{
    ConstructorName, ExternalName, FunctionName, Identifier, RawIdentifier, Variable,
};
use crate::parser::base::{Error, Result, State};
use crate::parser::lex::{
    lexer::{LocatedToken, Request},
    token::Token,
};

// TODO: These are more appropriate for lexer.
pub fn identifier(state: &mut State) -> Result<RawIdentifier> {
    let LocatedToken {
        token: Token::Identifier(str),
        position,
    } = state.request_token(Request::Identifier)?
    else {
        unreachable!()
    };
    let identifier = RawIdentifier::new(str, position, state.interner());
    Ok(identifier)
}

pub fn constructor_name(state: &mut State) -> Result<ConstructorName> {
    let id = identifier(state)?;
    if !id.first_char(state.interner()).is_ascii_uppercase() {
        return Err(Error::ExpectedTypeConstructor { received: id });
    }
    Ok(id)
}

pub fn variable(state: &mut State) -> Result<Variable> {
    let id = identifier(state)?;
    if !id.first_char(state.interner()).is_ascii_lowercase() {
        return Err(Error::ExpectedTypeConstructor { received: id });
    }
    Ok(id)
}

pub fn function_name(state: &mut State) -> Result<FunctionName> {
    variable(state)
}

pub fn foreign_function_name(state: &mut State) -> Result<ExternalName> {
    state.request_token(Request::Quote)?;
    let external_name = variable(state)?;
    state.request_token(Request::Quote)?;
    Ok(external_name)
}
