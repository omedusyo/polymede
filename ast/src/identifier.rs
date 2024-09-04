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
pub struct Variable(pub RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeVariable(pub RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionName(pub RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExternalName(pub RawIdentifier);

// Identifier that starts with uppercase letter.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeName(pub RawIdentifier);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ConstructorName(pub RawIdentifier);

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

pub fn duplicates<I: Identifier + Clone + Hash + Eq>(identifiers: &[I]) -> Vec<I> {
    use std::collections::HashSet;
    let mut seen: HashSet<&I> = HashSet::new();
    let mut duplicates: Vec<I> = vec![];

    for id in identifiers {
        if !seen.insert(id) {
            // id was already present.
            duplicates.push(id.clone());
        }
    }
    duplicates
}

// ===Identifier===
impl Identifier for RawIdentifier {
    fn symbol(&self) -> Symbol {
        self.symbol
    }
    fn position(&self) -> Position {
        self.position
    }
}

impl Identifier for Variable {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for TypeVariable {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for FunctionName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for ExternalName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for TypeName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

impl Identifier for ConstructorName {
    fn symbol(&self) -> Symbol {
        self.0.symbol
    }
    fn position(&self) -> Position {
        self.0.position
    }
}

// ===Into===
impl From<Variable> for RawIdentifier {
    fn from(id: Variable) -> RawIdentifier {
        id.0
    }
}

impl From<TypeVariable> for RawIdentifier {
    fn from(id: TypeVariable) -> RawIdentifier {
        id.0
    }
}

impl From<TypeName> for RawIdentifier {
    fn from(id: TypeName) -> RawIdentifier {
        id.0
    }
}

impl From<FunctionName> for RawIdentifier {
    fn from(id: FunctionName) -> RawIdentifier {
        id.0
    }
}

impl From<ExternalName> for RawIdentifier {
    fn from(id: ExternalName) -> RawIdentifier {
        id.0
    }
}

impl From<ConstructorName> for RawIdentifier {
    fn from(id: ConstructorName) -> RawIdentifier {
        id.0
    }
}

// ===From===
impl From<RawIdentifier> for Variable {
    fn from(id: RawIdentifier) -> Variable {
        Variable(id)
    }
}

impl From<RawIdentifier> for TypeVariable {
    fn from(id: RawIdentifier) -> TypeVariable {
        TypeVariable(id)
    }
}

impl From<RawIdentifier> for FunctionName {
    fn from(id: RawIdentifier) -> FunctionName {
        FunctionName(id)
    }
}

impl From<RawIdentifier> for ExternalName {
    fn from(id: RawIdentifier) -> ExternalName {
        ExternalName(id)
    }
}

impl From<RawIdentifier> for TypeName {
    fn from(id: RawIdentifier) -> TypeName {
        TypeName(id)
    }
}

impl From<RawIdentifier> for ConstructorName {
    fn from(id: RawIdentifier) -> ConstructorName {
        ConstructorName(id)
    }
}
