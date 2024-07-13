use crate::parser::{
    identifier::{Variable, ConstructorName},
    base::Type,
};

pub type Result<A> = std::result::Result<A, Error>;

#[derive(Debug)]
pub enum Error {
    TypeConstructorDoesntExist { type_name: Variable },
    TypeConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
    UndefinedTypeVaraible { variable: Variable },
    NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration { variable: Variable },

    VariableOutOfScope { variable: Variable },
    VariableDoesntHaveExpectedType { expected_type: Type, received_type: Type },

    TypeAnnotationDoesntMatchExpectedType { expected_type: Type, type_annotation: Type },
    TermIsConstructorButExpectedTypeIsNot { expected_type: Type },
    TermIsLambdaButExpectedTypeIsNotArrowType { expected_type: Type },
    ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name: ConstructorName, type_name: Variable },
    ConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
    LambdaHasWrongNumberOfArguments { expected: usize, received: usize },
    AttemptToMatchNonEnumerableType { received: Type },
    PatternHasWrongNumberOfArguments { expected: usize, received: usize },
    AttemptToFoldNonIndType { received: Type },
}

