use crate::identifier::{
    ConstructorName, FunctionName, Identifier, RawIdentifier, TypeName, TypeVariable, Variable,
};
use crate::parser::lex::{
    lexer::{LocatedToken, Position, Request},
    token::SeparatorSymbol,
};
use crate::parser::{
    base::{Error, Result, State},
    identifier::identifier,
};

pub fn comma(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } =
        state.request_token(Request::Separator(SeparatorSymbol::Comma))?;
    Ok(position)
}

pub fn or_separator(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } =
        state.request_token(Request::Separator(SeparatorSymbol::Or))?;
    Ok(position)
}

// TODO: Can we get rid of this?
pub fn do_nothing(state: &mut State) -> Result<()> {
    state.consume_whitespace_or_fail_when_end()
}

pub enum TypeNameOrConstructorName {
    Variable(TypeVariable),
    TypeName(TypeName),
}

pub fn type_name_or_type_variable(state: &mut State) -> Result<TypeNameOrConstructorName> {
    let id: RawIdentifier = identifier(state)?;
    let c = id.first_char(state.interner());
    if c.is_ascii_uppercase() {
        // constructor
        Ok(TypeNameOrConstructorName::TypeName(TypeName(id)))
    } else if c.is_ascii_lowercase() {
        // variable
        Ok(TypeNameOrConstructorName::Variable(TypeVariable(id)))
    } else {
        Err(Error::ExpectedTypeConstructorOrTypeVar { received: id })
    }
}

pub enum StartTerm {
    TypeAnnotation,
    Int(i32),
    Float(f32),
    StringLiteral(String),
    VariableUse(Variable),
    FunctionApplication(FunctionName),
    ConstructorConstant(ConstructorName),
    ConstructorApplication(ConstructorName),
    Match,
    Fold,
    Let,
    Apply,
    Lambda,
    Pure,
    Do,
    Receive,
}

pub enum StartPattern {
    Variable(Variable),
    ConstructorConstant(ConstructorName),
    ConstructorApplication(ConstructorName),
    Int(i32),
    Anything(RawIdentifier),
}

pub fn start_pattern(state: &mut State) -> Result<StartPattern> {
    if let Some(x) = state.commit_if_next_token_int()? {
        return Ok(StartPattern::Int(x));
    }

    let id: RawIdentifier = identifier(state)?;
    let c = id.first_char(state.interner());
    if c.is_ascii_uppercase() {
        // constructor
        if state.is_next_token_open_paren()? {
            Ok(StartPattern::ConstructorApplication(ConstructorName(id)))
        } else {
            // constructor constant
            Ok(StartPattern::ConstructorConstant(ConstructorName(id)))
        }
    } else if c.is_ascii_lowercase() {
        // variable
        Ok(StartPattern::Variable(Variable(id)))
    } else if c == '_' {
        Ok(StartPattern::Anything(id))
    } else {
        Err(Error::ExpectedTypeConstructorOrTypeVarOrAnythingInPattern { received: id })
    }
}

// TODO: This needs a refactor.
//       For type annotation it doesn't consume the `#` symbol,
//       but for fold/match/let/apply/fn it does consume the keyword (we're taking advantage of
//       that these keywords are valid identifiers)
pub fn start_term(state: &mut State) -> Result<StartTerm> {
    if state.is_next_token_start_type_annotation()? {
        return Ok(StartTerm::TypeAnnotation);
    }

    if let Some(s) = state.commit_if_next_token_string_literal()? {
        return Ok(StartTerm::StringLiteral(s));
    }
    if let Some(x) = state.commit_if_next_token_int()? {
        return Ok(StartTerm::Int(x));
    }
    if let Some(x) = state.commit_if_next_token_f32()? {
        return Ok(StartTerm::Float(x));
    }

    let id: RawIdentifier = identifier(state)?;
    let c = id.first_char(state.interner());
    if c.is_ascii_uppercase() {
        // constructor
        if state.is_next_token_open_paren()? {
            Ok(StartTerm::ConstructorApplication(ConstructorName(id)))
        } else {
            // constructor constant
            Ok(StartTerm::ConstructorConstant(ConstructorName(id)))
        }
    } else if c.is_ascii_lowercase() {
        // TODO: Checking for fold/match here is really wrong. This should be the job for the lexer.
        match id.str(state.interner()) {
            "fold" => Ok(StartTerm::Fold),
            "match" => Ok(StartTerm::Match),
            "let" => Ok(StartTerm::Let),
            "apply" => Ok(StartTerm::Apply),
            "fn" => Ok(StartTerm::Lambda),
            "pure" => Ok(StartTerm::Pure),
            "do" => Ok(StartTerm::Do),
            "receive" => Ok(StartTerm::Receive),
            _ => {
                if state.is_next_token_open_paren()? || state.is_next_token_open_angle()? {
                    Ok(StartTerm::FunctionApplication(FunctionName(id)))
                } else {
                    Ok(StartTerm::VariableUse(Variable(id)))
                }
            }
        }
    } else {
        Err(Error::ExpectedTerm { received: id })
    }
}
