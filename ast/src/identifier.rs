use crate::parser::lex::lexer::Position;
use std::hash::{Hash, Hasher};

use string_interner::{backend::StringBackend, DefaultSymbol, StringInterner};

pub type Interner = StringInterner<StringBackend>;
pub type Symbol = DefaultSymbol;

pub fn interner() -> Interner {
    StringInterner::default()
}

#[derive(Debug, Clone)]
pub struct RawIdentifier {
    symbol: Symbol,
    position: Position,
}

// Identifier that starts with lowercase letter.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MyVariable(RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MyFunctionName(RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MyExternalName(RawIdentifier);

// Identifier that starts with uppercase letter.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MyTypeName(RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MyConstructorName(RawIdentifier);

impl RawIdentifier {
    pub fn new(str: String, position: Position, interner: &mut Interner) -> Self {
        let symbol = interner.get_or_intern(str);
        Self { symbol, position }
    }

    pub fn position(&self) -> Position {
        self.position
    }
}

pub trait Identifier {
    fn symbol(&self) -> Symbol;
    fn position(&self) -> Position;

    fn str<'interner: 'id, 'id>(&'id self, interner: &'interner Interner) -> &'id str {
        match interner.resolve(self.symbol()) {
            Some(str) => str,
            None => unreachable!(),
        }
    }

    fn first_char(&self, state: &Interner) -> char {
        // identifier is guaranteed to be non-empty
        self.str(state).chars().nth(0).unwrap()
    }
}

impl PartialEq for RawIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl Eq for RawIdentifier {}

impl Hash for RawIdentifier {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.symbol.hash(state)
    }
}

pub fn duplicates<T: Identifier + Clone + Hash + Eq>(identifiers: &[T]) -> Vec<T> {
    use std::collections::HashSet;
    let mut seen: HashSet<&T> = HashSet::new();
    let mut duplicates: Vec<T> = vec![];

    for id in identifiers {
        if !seen.insert(id) {
            // id was already present.
            duplicates.push(id.clone());
        }
    }
    duplicates
}

impl Identifier for RawIdentifier {
    fn symbol(&self) -> Symbol {
        self.symbol
    }
    fn position(&self) -> Position {
        self.position
    }
}

impl Identifier for MyVariable {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for MyFunctionName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for MyExternalName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for MyTypeName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for MyConstructorName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

// ====Old====

// Identifier that starts with lowercase letter.
pub type Variable = RawIdentifier;
pub type FunctionName = Variable;
pub type ExternalName = Variable;
// Identifier that starts with uppercase letter.
pub type ConstructorName = RawIdentifier;
