use crate::parser::lex::lexer::Position;
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
pub type ExternalName = Variable;
// Identifier that starts with uppercase letter.
pub type ConstructorName = Identifier;

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

    pub fn position(&self) -> Position {
        self.position
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
