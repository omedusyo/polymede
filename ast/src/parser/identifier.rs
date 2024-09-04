use crate::identifier;
use crate::identifier::{
    ConstructorName, ExternalName, FunctionName, Identifier, RawIdentifier, TypeName, TypeVariable,
    Variable,
};
use crate::parser::lex::{
    lexer::{LocatedToken, Request},
    token::Token,
};
use crate::parser::{
    base::{Error, Result, State},
    combinator::{
        delimited_nonempty_sequence_to_vector, delimited_possibly_empty_sequence_to_vector,
    },
    special::comma,
};
use std::hash::Hash;

// TODO: These are more appropriate for lexer.
pub fn identifier<I: From<RawIdentifier>>(state: &mut State) -> Result<I> {
    let LocatedToken {
        token: Token::Identifier(str),
        position,
    } = state.request_token(Request::Identifier)?
    else {
        unreachable!()
    };
    let identifier = RawIdentifier::new(str, position, state.interner());
    Ok(identifier.into())
}

pub fn uppercase_identifier<I: Identifier + From<RawIdentifier>>(state: &mut State) -> Result<I> {
    let id: RawIdentifier = identifier(state)?;
    if !id.first_char(state.interner()).is_ascii_uppercase() {
        return Err(Error::ExpectedVariable { received: id });
    }
    Ok(id.into())
}

pub fn lowercase_identifier<I: Identifier + From<RawIdentifier>>(state: &mut State) -> Result<I> {
    let id: RawIdentifier = identifier(state)?;
    if !id.first_char(state.interner()).is_ascii_lowercase() {
        return Err(Error::ExpectedVariable { received: id });
    }
    Ok(id.into())
}

pub fn constructor_name(state: &mut State) -> Result<ConstructorName> {
    uppercase_identifier(state)
}

pub fn type_name(state: &mut State) -> Result<TypeName> {
    uppercase_identifier(state)
}

pub fn variable(state: &mut State) -> Result<Variable> {
    lowercase_identifier(state)
}

pub fn type_variable(state: &mut State) -> Result<TypeVariable> {
    lowercase_identifier(state)
}

pub fn function_name(state: &mut State) -> Result<FunctionName> {
    lowercase_identifier(state)
}

pub fn foreign_function_name(state: &mut State) -> Result<ExternalName> {
    state.request_token(Request::Quote)?;
    let external_name: ExternalName = lowercase_identifier(state)?;
    state.request_token(Request::Quote)?;
    Ok(external_name)
}

// ===Parameters===
// Parses a non-empty sequence of identifiers separated by comma
//   x1, x2, x3
// Also checks that no two identifiers are equal.
pub fn parameter_non_empty_sequence<
    I: Identifier + Clone + Eq + Hash + From<RawIdentifier> + Into<RawIdentifier>,
>(
    state: &mut State,
) -> Result<Vec<I>> {
    let ids = delimited_nonempty_sequence_to_vector(state, lowercase_identifier, comma)?;
    let duplicate_ids = identifier::duplicates(&ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames {
            duplicates: duplicate_ids.into_iter().map(Into::into).collect(),
        })
    }
}

pub fn parameter_possibly_empty_sequence<
    I: Identifier + Clone + Eq + Hash + From<RawIdentifier> + Into<RawIdentifier>,
>(
    state: &mut State,
) -> Result<Vec<I>> {
    let ids = delimited_possibly_empty_sequence_to_vector(state, lowercase_identifier, comma)?;
    let duplicate_ids = identifier::duplicates(&ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames {
            duplicates: duplicate_ids.into_iter().map(Into::into).collect(),
        })
    }
}
