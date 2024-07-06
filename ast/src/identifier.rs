use crate::lexer::Position;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub struct Identifier {
    // TODO: Consider making this an interned String
    str: String,
    position: Position,
}

// Identifier that starts with lowercase letter.
pub type Variable = Identifier;
// Identifier that starts with uppercase letter.
pub type ConstructorName = Identifier;

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
