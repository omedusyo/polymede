use crate::parser::lex::{
    lexer::{Position, LocatedToken, Request},
    token::Token,
};
use crate::parser::base::{State, Result, Error};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub struct Identifier {
    // TODO: Consider making this an interned String
    str: String,
    position: Position,
}

// Identifier that starts with lowercase letter.
pub type Variable = Identifier;
pub type FunctionName = Variable;
// Identifier that starts with uppercase letter.
pub type ConstructorName = Identifier;

// TODO: These are more appropriate for lexer.
pub fn identifier(state: &mut State) -> Result<Identifier> {
    let LocatedToken { token: Token::Identifier(str), position } = state.request_token(Request::Identifier)? else { unreachable!() };
    let identifier = Identifier::new(str, position);
    Ok(identifier)
}

pub fn constructor_name(state: &mut State) -> Result<ConstructorName> {
    let id = identifier(state)?;
    if !id.first_char().is_ascii_uppercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }
    Ok(id)
}

pub fn variable(state: &mut State) -> Result<Variable> {
    let id = identifier(state)?;
    if !id.first_char().is_ascii_lowercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }
    Ok(id)
}

pub fn function_name(state: &mut State) -> Result<FunctionName> {
    variable(state)
}

impl Identifier {
    pub fn new(str: String, position: Position) -> Self {
        Self { str, position }
    }

    pub fn str(&self) -> &str {
        &self.str
    }

    pub fn first_char(&self) -> char {
        // identifier is guaranteed to be non-empty
        let c = self.str.chars().nth(0).unwrap();
        c
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.str == other.str
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.str.hash(state)
    }
}
 
pub fn duplicates(identifiers: &[Identifier]) -> Vec<Identifier> {
    use std::collections::HashSet;
    let mut seen: HashSet<&Identifier> = HashSet::new();
    let mut duplicates: Vec<Identifier> = vec![];

    for id in identifiers {
        if !seen.insert(id) {
            // id was already present.
            duplicates.push(id.clone());
        }
    }
    duplicates
}
