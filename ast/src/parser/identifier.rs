use crate::parser::lex::{
    lexer::{Position, LocatedToken, Request},
    token::Token,
};
use crate::parser::base::{State, Result, Error};
use std::hash::{Hash, Hasher};

use string_interner::{
    StringInterner,
    backend::StringBackend,
    DefaultSymbol,
};

pub type Interner = StringInterner<StringBackend>;
pub type Symbol = DefaultSymbol;

pub fn interner() -> Interner {
    StringInterner::default()
}

#[derive(Debug, Clone)]
pub struct Identifier {
    symbol: Symbol,
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
    let identifier = Identifier::new(str, position, state.interner());
    Ok(identifier)
}

pub fn constructor_name(state: &mut State) -> Result<ConstructorName> {
    let id = identifier(state)?;
    if !id.first_char(&state.interner()).is_ascii_uppercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }
    Ok(id)
}

pub fn variable(state: &mut State) -> Result<Variable> {
    let id = identifier(state)?;
    if !id.first_char(&state.interner()).is_ascii_lowercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }
    Ok(id)
}

pub fn function_name(state: &mut State) -> Result<FunctionName> {
    variable(state)
}

impl Identifier {
    pub fn new(str: String, position: Position, interner: &mut Interner) -> Self {
        let symbol = interner.get_or_intern(str);
        Self { symbol, position }
    }

    pub fn str<'interner: 'id, 'id>(&'id self, interner: &'interner Interner) -> &'id str {
        match interner.resolve(self.symbol) {
            Some(str) => str,
            None => unreachable!()
        }
    }

    pub fn first_char(&self, state: &Interner) -> char {
        // identifier is guaranteed to be non-empty
        let c = self.str(state).chars().nth(0).unwrap();
        c
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl Eq for Identifier {}

impl Hash for Identifier {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.symbol.hash(state)
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
