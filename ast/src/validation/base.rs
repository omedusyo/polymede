use crate::base::Type;
use crate::identifier::{Variable, ConstructorName, FunctionName};
use crate::parser::show::Show;

pub type Result<A> = std::result::Result<A, Error>;

#[derive(Debug)]
pub enum Error {
    TypeConstructorDoesntExist { type_name: Variable },
    TypeConstructorIsApplliedToWrongNumberOfArguments { expected: usize, received: usize },
    UndefinedTypeVaraible { variable: Variable },
    NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration { variable: Variable },
    MsgTypeIsNotValueType { received_type: Type },
    MsgTypeCantHaveTypeParameters,

    RunExpressionDoesntHaveExpectedCommandType { received: Type },

    VariableOutOfScope { variable: Variable },
    VariableDoesntHaveExpectedType { variable: Variable, expected_type: Type, received_type: Type },
    TypeAnnotationDoesntMatchExpectedType { expected_type: Type, received_type: Type },
    TermIsConstructorButExpectedTypeIsNot { expected_type: Type },
    TermIsLambdaButExpectedTypeIsNotArrowType { expected_type: Type },
    TermIsCommandButExpectedTypeIsNotCommandType { expected_type: Type },
    ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name: ConstructorName, type_name: Variable },
    ConstructorDoesntExist { constructor_name: ConstructorName },
    ConstructorIsAppliedToWrongNumberOfArguments { constructor_name: ConstructorName, expected: usize, received: usize },
    LambdaHasWrongNumberOfArguments { expected: usize, received: usize },
    LambdaOutputTypeDoesntMatchExpectedType { expected_type: Type, received_type: Type },
    AttemptToMatchNonEnumerableType { received_type: Type },
    PatternHasWrongNumberOfArguments { expected: usize, received: usize },
    AttemptToFoldNonIndType { received_type: Type },
    ApplyingWrongNumberOfArgumentsToLambda { expected: usize, received: usize },
    FunctionDoesntExist { function_name: FunctionName },
    ApplyingWrongNumberOfArgumentsToFunction { function_name: FunctionName, expected: usize, received: usize },
    ApplyingWrongNumberOfTypeArgumentsToFunction { function_name: FunctionName, expected: usize, received: usize },
    FunctionOutputTypeDoesntMatchExpectedType { function_name: FunctionName, expected_type: Type, received_type: Type },
    TermDoesntHaveExpectedArrowType { received: Type },
    AttemptToExecuteNonCommand { received: Type },
    ReturnNonCommandInDoExpression { received: Type },
    ReceiveExpressionIsExpectedToBeNonMessageType { expected_type: Type, msg_cmd_type: Type },

    UnableToInferTypeOfMatch,
    UnableToInferTypeOfFold,
    UnableToInferTypeOfLambda,
    UnableToInferTypeOfConstructor,
}

#[derive(Debug)]
pub enum ErrorWithLocation {
    TypeDeclaration(Variable, Error),
    FunctionDeclaration(FunctionName, Error),
    RunDeclaration(Error),
}

impl Error {
    pub fn show(&self, sh: &Show) -> String {
        use Error::*;
        match self {
            TypeConstructorDoesntExist { type_name } => format!("Type Constructor '{}' doesn't exist.", sh.show_identifier(type_name)),
            TypeConstructorIsApplliedToWrongNumberOfArguments { expected, received } => format!("Type Constructor is applied to {} arguments but expects {}.", received, expected),
            UndefinedTypeVaraible { variable } => format!("Undefined type variable '{}'.", sh.show_identifier(variable)),
            NegativeOccuranceOfRecursiveTypeVariableInInductiveDeclaration { variable } => format!("Negative occurance of self type variable {} in inductive type declaration.", sh.show_identifier(variable)),
            MsgTypeIsNotValueType { received_type } => format!("Message type {} is not a value type.", sh.show_type(received_type)),
            MsgTypeCantHaveTypeParameters => format!("Message type can't have type parameters."),

            FunctionOutputTypeDoesntMatchExpectedType { function_name, expected_type, received_type } => format!("Function's '{}' output type is {} but is expected to be {}.", sh.show_identifier(function_name), sh.show_type(received_type), sh.show_type(expected_type)),

            RunExpressionDoesntHaveExpectedCommandType { received } => format!("Run expression is supposed to be a command but has type {}", sh.show_type(received)),

            VariableOutOfScope { variable } => format!("Variable '{}' is out of scope.", sh.show_identifier(variable)),
            VariableDoesntHaveExpectedType { variable, expected_type, received_type } => format!("Variable '{}' has type {} but is expected to be {}.", sh.show_identifier(variable), sh.show_type(received_type), sh.show_type(expected_type)),
            TypeAnnotationDoesntMatchExpectedType { expected_type, received_type } => format!("Type annotation is {} but is expected to be {}.", sh.show_type(received_type), sh.show_type(expected_type)),
            TermIsConstructorButExpectedTypeIsNot { expected_type } => format!("Term is a constructor, but its expected type is {}.", sh.show_type(expected_type)),
            TermIsLambdaButExpectedTypeIsNotArrowType { expected_type } => format!("Term is a lambda, but its expected type is {}.", sh.show_type(expected_type)),
            TermIsCommandButExpectedTypeIsNotCommandType { expected_type } => format!("Term is a command, but its expected type is {}.", sh.show_type(expected_type)),
            ConstructorDoesntBelongToExpectedTypeDeclaration { constructor_name, type_name } => format!("Constructor '{}' doesn't belong to type declaration '{}'.", sh.show_identifier(constructor_name), sh.show_identifier(type_name)),
            ConstructorDoesntExist { constructor_name } => format!("Constructor '{}' doesn't exist.", sh.show_identifier(constructor_name)),
            ConstructorIsAppliedToWrongNumberOfArguments { constructor_name, expected, received } => format!("Constructor '{}' is applied to {} arguments but expects {}.", sh.show_identifier(constructor_name), received, expected),
            LambdaHasWrongNumberOfArguments { expected, received } => format!("Lambda has {} arguments but expects {}.", received, expected),
            LambdaOutputTypeDoesntMatchExpectedType { expected_type, received_type } => format!("Lambda's output type is {} but is expected to be {}.", sh.show_type(received_type), sh.show_type(expected_type)),
            AttemptToMatchNonEnumerableType { received_type } => format!("Attempt to match on non-enumerable type {}.", sh.show_type(received_type)),
            PatternHasWrongNumberOfArguments { expected, received } => format!("Pattern has {} arguments but is expected to have {}.", received, expected),
            AttemptToFoldNonIndType { received_type: received } => format!("Attempt to fold on non-inductive type {}.", sh.show_type(received)),
            ApplyingWrongNumberOfArgumentsToLambda { expected, received } => format!("Applying lambda to {} arguments but it has {} parameters.", received, expected),
            FunctionDoesntExist { function_name } => format!("Function '{}' doesn't exist.", sh.show_identifier(function_name)),
            ApplyingWrongNumberOfArgumentsToFunction { function_name, expected, received } => format!("Applying function '{}' to {} arguments but it has {} parameters.", sh.show_identifier(function_name), received, expected),
            ApplyingWrongNumberOfTypeArgumentsToFunction { function_name, expected, received } => format!("Applying function '{}' to {} type arguments but it has {} type-parameters.", sh.show_identifier(function_name), received, expected),
            FunctionOutputTypeDoesntMatchExpectedType { function_name, expected_type, received_type } => format!("Function's '{}' output type is {} but is expected to be {}.", sh.show_identifier(function_name), sh.show_type(received_type), sh.show_type(expected_type)),
            TermDoesntHaveExpectedArrowType { received } => format!("Term has type {}, but is expected to be an Arrow type.", sh.show_type(received)),
            AttemptToExecuteNonCommand { received } => format!("Term is supposed to be a command but has type {}", sh.show_type(received)),
            ReturnNonCommandInDoExpression { received } => format!("Can't return term of type {} in a do expression", sh.show_type(received)),
            ReceiveExpressionIsExpectedToBeNonMessageType { expected_type, msg_cmd_type: msg_type } => format!("Receive expression's expected type {} does not match the command-message type {}", sh.show_type(expected_type), sh.show_type(msg_type)),

            UnableToInferTypeOfMatch => format!("Unable to infer type of match expression."),
            UnableToInferTypeOfFold => format!("Unable to infer type of fold expression."),
            UnableToInferTypeOfLambda => format!("Unable to infer type of lambda expression."),
            UnableToInferTypeOfConstructor => format!("Unable to infer type of constructor."),
        }
    }
}

impl ErrorWithLocation {
    pub fn show(&self, sh: &Show) -> String {
        match self {
            Self::TypeDeclaration(name, e) => format!("In type {} = ...: {}", sh.show_identifier(name), e.show(sh)),
            Self::FunctionDeclaration(name, e) => format!("In fn {} = ...: {}", sh.show_identifier(name), e.show(sh)),
            Self::RunDeclaration(e) => format!("In run ...: {}", e.show(sh)),
        }
    }
}
