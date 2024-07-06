use crate::lexer;
use crate::lexer::{LocatedToken, Position, Request};
use crate::token::{Token, SeparatorSymbol, Keyword};
use crate::base::{Program, PrimitiveType};
use crate::identifier;
use crate::identifier::{Identifier, Variable, FunctionName, ConstructorName};

type Result<A> = std::result::Result<A, Error>;

#[derive(Debug, PartialEq)]
enum Type {
    VariableUse(Variable),
    TypeApplication(ConstructorName, Vec<Type>),
    Arrow(Box<FunctionType>),
}

#[derive(Debug, PartialEq)]
struct FunctionType {
    input_types: Vec<Type>,
    output_type: Type,
}

#[derive(Debug)]
struct ConstructorDeclaration {
    name: Identifier,
    parameters: Vec<Type>,
}

#[derive(Debug)]
struct EnumDeclaration {
    name: ConstructorName,
    type_parameters: Vec<Variable>,
    constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
struct IndDeclaration {
    name: ConstructorName,
    type_parameters: Vec<Variable>,
    recursive_type_var: Variable,
    constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
enum TypeDeclaration {
    Enum(EnumDeclaration),
    Ind(IndDeclaration),
}

#[derive(Debug)]
struct FunctionDeclaration {
    name: FunctionName,
    type_parameters: Vec<Variable>,
    function: Function,
}

#[derive(Debug)]
struct Function {
    type_: FunctionType,
    parameters: Vec<Variable>,
    body: Term,
}

struct LetDeclaration {
    name: Variable,
    type_parameters: Vec<Variable>,
    body: TypedTerm,
}

struct TypedTerm {
    type_: Type,
    term: Term
}

#[derive(Debug)]
enum Term {
    VariableUse(Variable),
    FunctionApplication(Variable, Vec<Term>),
    ConstructorUse(ConstructorName, Vec<Term>),
    Match(Box<Term>, Vec<PatternBranch>),
    Fold(Box<Term>, Vec<PatternBranch>),
    Lambda(Box<Function>),
    LambdaApplication(Box<Term>, Vec<Term>),
    Let(Vec<(Variable, Term)>, Box<Term>),
}

#[derive(Debug)]
struct LocalLetBinding {
    bindings: Vec<(Variable, Term)>,
    body: Term,
}

#[derive(Debug)]
struct PatternBranch {
    pattern: Pattern,
    body: Term,
}

#[derive(Debug)]
pub enum Pattern {
    Constructor(ConstructorName, Vec<Pattern>),
    Variable(Variable),
    Anything(Identifier),
}


// Note that this is not a closure type. It's simply a function pointer.
type Parser<A> = fn(&mut State) -> Result<A>;

#[derive(Debug)]
pub enum Error {
    LexError(lexer::Error),
    ExpectedTypeConstructorOrTypeVar { received: Identifier },
    ExpectedTypeConstructorOrTypeVarOrAnythingInPattern { received: Identifier },
    ExpectedTerm { received: Identifier },
    ExpectedTypeConstructor { received: Identifier },
    ExpectedTypeVar { received: Identifier },
    DuplicateVariableNames { duplicates: Vec<Identifier> },
    FunctionHasDifferentNumberOfParametersThanDeclaredInItsType { declared_in_type: usize, parameters: usize },
}


#[derive(Debug)]
pub struct State<'a> {
    lexer_state: lexer::State<'a>
}

impl <'state> State<'state> {
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self { lexer_state: lexer::State::new(str) }
    }

    pub fn request_token(&mut self, request: Request) -> Result<LocatedToken> {
        self.lexer_state.request(request).map_err(Error::LexError)
    }

    pub fn request_keyword(&mut self, keyword: Keyword) -> Result<LocatedToken> {
        self.request_token(Request::Keyword(keyword))
    }

    pub fn consume_optional_or(&mut self) -> Result<()> {
        self.lexer_state.consume_optional_or().map_err(Error::LexError)
    }

    pub fn consume_optional_comma(&mut self) -> Result<()> {
        self.lexer_state.consume_optional_comma().map_err(Error::LexError)
    }

    pub fn is_next_token_open_paren(&mut self) -> Result<bool> {
        self.lexer_state.is_next_token_open_paren().map_err(Error::LexError)
    }

    pub fn commit_if_next_token_forall(&mut self) -> Result<bool> {
        self.lexer_state.commit_if_next_token_forall().map_err(Error::LexError)
    }

    pub fn lookahead_char(&self) -> Result<char> {
        self.lexer_state.read_char_or_fail_when_end().map_err(Error::LexError)
    }

    pub fn clone(&self) -> State<'state> {
        Self { lexer_state: self.lexer_state.clone() }
    }
}

// ===Parser Combinator===
// needs `p` to succeed atleast once
// parses
//   p d p d p d p d p
fn delimited_nonempty_sequence<S, A, D>(
    state: &mut State,
    mut s: S, // initial state of the computation
    p: Parser<A>,
    delim: Parser<D>,
    combine: fn(S, A) -> S)
 -> Result<S>
{
    let a = p(state)?;
    s = combine(s, a);

    loop {
        let saved_state = state.clone();
        match delim(state) {
            Ok(_d) => {
                let a = p(state)?;
                s = combine(s, a);
            },
            Err(_) => {
                // backtrack.
                *state = saved_state;
                break
            }
        }
    }
    Ok(s)
}

fn delimited_possibly_empty_sequence<S, A, D>(
    state: &mut State,
    mut s: S, // initial state of the computation
    p: Parser<A>,
    delim: Parser<D>,
    combine: fn(S, A) -> S)
 -> Result<S>
{
    let a = { 
        let saved_state = state.clone();
        match p(state) {
            Ok(a) => a,
            Err(_err) => {
                // backtrack
                *state = saved_state;
                return Ok(s)
            }
        }
    };
    s = combine(s, a);

    loop {
        let saved_state = state.clone();
        match delim(state) {
            Ok(_d) => {
                let a = p(state)?;
                s = combine(s, a);
            },
            Err(_err) => {
                // backtrack.
                *state = saved_state;
                break
            }
        }
    }
    Ok(s)
}

fn delimited_nonempty_sequence_to_vector<A, D>(
    state: &mut State,
    p: Parser<A>,
    delim: Parser<D>)
    -> Result<Vec<A>> 
{
    delimited_nonempty_sequence(
        state,
        vec![],
        p,
        delim,
        |mut xs: Vec<A>, x: A| {
            xs.push(x);
            xs
        }
    )
}

fn delimited_possibly_empty_sequence_to_vector<A, D>(
    state: &mut State,
    p: Parser<A>,
    delim: Parser<D>)
    -> Result<Vec<A>> 
{
    delimited_possibly_empty_sequence(
        state,
        vec![],
        p,
        delim,
        |mut xs: Vec<A>, x: A| {
            xs.push(x);
            xs
        }
    )
}

// ===Specific Parsers===
fn comma(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } = state.request_token(Request::Separator(SeparatorSymbol::Comma))?;
    Ok(position)
}

fn or_separator(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } = state.request_token(Request::Separator(SeparatorSymbol::Or))?;
    Ok(position)
}

fn and_separator(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } = state.request_token(Request::Separator(SeparatorSymbol::And))?;
    Ok(position)
}

// ===Identifiers===
// TODO: These are more appropriate for lexer.
fn identifier(state: &mut State) -> Result<Identifier> {
    let LocatedToken { token: Token::Identifier(str), position } = state.request_token(Request::Identifier)? else { unreachable!() };
    let identifier = Identifier::new(str, position);
    Ok(identifier)
}

fn constructor_name(state: &mut State) -> Result<ConstructorName> {
    let id = identifier(state)?;
    if !id.first_char().is_ascii_uppercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }
    Ok(id)
}

fn variable(state: &mut State) -> Result<Variable> {
    let id = identifier(state)?;
    if !id.first_char().is_ascii_lowercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }
    Ok(id)
}

fn function_name(state: &mut State) -> Result<FunctionName> {
    variable(state)
}

enum VariableOrConstructorName {
    Variable(Variable),
    ConstructorName(ConstructorName)
}

fn constructor_name_or_variable(state: &mut State) -> Result<VariableOrConstructorName> {
    let id = identifier(state)?;
    let c = id.first_char();
    if c.is_ascii_uppercase() {
        // constructor
        Ok(VariableOrConstructorName::ConstructorName(id))
    } else if c.is_ascii_lowercase() {
        // variable
        Ok(VariableOrConstructorName::Variable(id))
    } else {
        Err(Error::ExpectedTypeConstructorOrTypeVar { received: id })
    }
}

enum StartTerm {
    VariableUse(Variable),
    FunctionApplication(Variable),
    ConstructorConstant(ConstructorName),
    ConstructorApplication(ConstructorName),
    Match,
    Fold,
    Let,
    Apply,
    Lambda,
}

fn start_term(state: &mut State) -> Result<StartTerm> {
    let id = identifier(state)?;
    let c = id.first_char();
    if c.is_ascii_uppercase() {
        // constructor
        if state.is_next_token_open_paren()? {
            Ok(StartTerm::ConstructorApplication(id))
        } else {
            // constructor constant
            Ok(StartTerm::ConstructorConstant(id))
        }
    } else if c.is_ascii_lowercase() {
        // TODO: Checking for fold/match here is really wrong. This should be the job for the lexer.
        match id.str() {
            "fold" => Ok(StartTerm::Fold),
            "match" => Ok(StartTerm::Match),
            "let" => Ok(StartTerm::Let),
            "apply" => Ok(StartTerm::Apply),
            "fn" => Ok(StartTerm::Lambda),
            _ => {
                if state.is_next_token_open_paren()? {
                    Ok(StartTerm::FunctionApplication(id))
                } else {
                    Ok(StartTerm::VariableUse(id))
                }
            },
        }
    } else {
        Err(Error::ExpectedTypeConstructorOrTypeVarOrAnythingInPattern { received: id })
    }
}

enum PatternIdentifier {
    Variable(Variable),
    ConstructorName(ConstructorName),
    Anything(Identifier),
}

fn pattern_identifier(state: &mut State) -> Result<PatternIdentifier> {
    let id = identifier(state)?;
    let c = id.first_char();
    if c.is_ascii_uppercase() {
        // constructor
        Ok(PatternIdentifier::ConstructorName(id))
    } else if c.is_ascii_lowercase() {
        // variable
        Ok(PatternIdentifier::Variable(id))
    } else if c == '_' {
        Ok(PatternIdentifier::Anything(id))
    } else {
        Err(Error::ExpectedTypeConstructorOrTypeVarOrAnythingInPattern { received: id })
    }
}

// Parses a non-empty sequence of identifiers separated by comma
//   x1, x2, x3
// Also checks that no two identifiers are equal.
fn parameter_non_empty_sequence(state: &mut State) -> Result<Vec<Identifier>> {
    let ids = delimited_nonempty_sequence_to_vector(state, variable, comma)?;
    let duplicate_ids = identifier::duplicates(&ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames { duplicates: duplicate_ids })
    }
}

fn parameter_possibly_empty_sequence(state: &mut State) -> Result<Vec<Identifier>> {
    let ids = delimited_possibly_empty_sequence_to_vector(state, variable, comma)?;
    let duplicate_ids = identifier::duplicates(&ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames { duplicates: duplicate_ids })
    }
}

// ===Types===
// Parses
//   x
//   Cons(T1, T2)
//   Fn(T1, T2 -> T)
fn type_(state: &mut State) -> Result<Type> {
    match constructor_name_or_variable(state)? {
        VariableOrConstructorName::Variable(variable) => Ok(Type::VariableUse(variable)),
        VariableOrConstructorName::ConstructorName(constructor_name) => {
            if state.is_next_token_open_paren()? {
                // A type constructor with multiple parameters.
                state.request_token(Request::OpenParen)?;
                if constructor_name.str() == "Fn" {
                    let fn_type = function_type(state)?;
                    state.request_token(Request::CloseParen)?;
                    Ok(Type::Arrow(Box::new(fn_type)))
                } else {
                    let type_args = type_nonempty_sequence(state)?;
                    state.request_token(Request::CloseParen)?;
                    Ok(Type::TypeApplication(constructor_name, type_args))
                }
            } else {
                // A type constant
                Ok(Type::TypeApplication(constructor_name, vec![]))
            }
        }
    }
}

// Parses   T1, T2, T3, T4 possibly empty
fn type_possibly_empty_sequence(state: &mut State) -> Result<Vec<Type>> {
    delimited_possibly_empty_sequence_to_vector(state, type_, comma)
}

fn type_nonempty_sequence(state: &mut State) -> Result<Vec<Type>> {
    delimited_nonempty_sequence_to_vector(state, type_, comma)
}

// Parses  T1, T2 -> T
fn function_type(state: &mut State) -> Result<FunctionType> {
    let input_types = type_possibly_empty_sequence(state)?;
    state.request_keyword(Keyword::Arrow)?;
    let output_type = type_(state)?;
    Ok(FunctionType { input_types, output_type })
}

// TODO: Check for uniqueness of constructor names.
fn constructor_declaration(state: &mut State) -> Result<ConstructorDeclaration> {
    let constructor_name = constructor_name(state)?;

    if state.is_next_token_open_paren()? {
        // Constructor with non-zero parameters
        state.request_token(Request::OpenParen)?;
        let parameter_types = type_nonempty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        Ok(ConstructorDeclaration { name: constructor_name, parameters: parameter_types, })
    } else {
        // Constant
        Ok(ConstructorDeclaration { name: constructor_name, parameters: vec![] })
    }
}

fn constructor_declaration_sequence(state: &mut State) -> Result<Vec<ConstructorDeclaration>> {
    state.consume_optional_or()?;
    delimited_nonempty_sequence_to_vector( state, constructor_declaration, or_separator)
}

fn type_declaration(state: &mut State) -> Result<TypeDeclaration> {
    state.request_keyword(Keyword::Type_)?;

    let constructor_name = constructor_name(state)?;

    let type_parameters: Vec<Variable> = if state.is_next_token_open_paren()? {
        state.request_token(Request::OpenParen)?;
        let params = parameter_non_empty_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        params
    } else {
        vec![]
    };

    state.request_keyword(Keyword::Eq)?;

    let LocatedToken { token: Token::Keyword(keyword), .. } = state.request_token(Request::TypeDeclarationKeyword)? else { unreachable!() };
    state.request_token(Request::OpenCurly)?;
    let declaration = match keyword {
        Keyword::Enum => {
            TypeDeclaration::Enum(EnumDeclaration {
                name: constructor_name,
                type_parameters,
                constructors: constructor_declaration_sequence(state)?
            })
        },
        Keyword::Ind => {
            let recursive_type_var = variable(state)?;

            state.request_token(Request::BindingSeparator)?;

            TypeDeclaration::Ind(IndDeclaration {
                name: constructor_name,
                type_parameters,
                recursive_type_var,
                constructors: constructor_declaration_sequence(state)?
            })
        },
        _ => unreachable!(),
    };
    state.request_token(Request::CloseCurly)?;

    Ok(declaration)
}

// ===Function/Let Declarations===
fn function_declaration(state: &mut State) -> Result<FunctionDeclaration> {
    state.request_keyword(Keyword::Fn)?;
    let function_name = function_name(state)?;
    state.request_keyword(Keyword::Eq)?;


    fn inner_function_declaration(state: &mut State, function_name: FunctionName, type_parameters: Vec<Variable>) -> Result<FunctionDeclaration> {
        let type_ = function_type_annotation(state)?;
        let function = function(state, type_)?;
        Ok(FunctionDeclaration { name: function_name, type_parameters, function })
    }

    if state.commit_if_next_token_forall()? {
        // forall encountered
        state.request_token(Request::OpenCurly)?;
        let type_parameters = parameter_non_empty_sequence(state)?;
        state.request_token(Request::BindingSeparator)?;

        let declaration = inner_function_declaration(state, function_name, type_parameters)?;

        state.request_token(Request::CloseCurly)?;
        Ok(declaration)
    } else {
        // no forall
        inner_function_declaration(state, function_name, vec![])
    }
}

fn function(state: &mut State, type_: FunctionType) -> Result<Function> {
    state.request_token(Request::OpenCurly)?;

    let parameters = parameter_possibly_empty_sequence(state)?;
    state.request_token(Request::BindingSeparator)?;
    let body = term(state)?;

    state.request_token(Request::CloseCurly)?;

    if type_.input_types.len() != parameters.len() {
        Err(Error::FunctionHasDifferentNumberOfParametersThanDeclaredInItsType { declared_in_type: type_.input_types.len(), parameters: parameters.len() })
    } else {
        Ok(Function { type_, parameters, body })
    }
}

fn let_declaration(state: &mut State) -> Result<LetDeclaration> {
    state.request_keyword(Keyword::Let)?;
    let name = function_name(state)?;
    state.request_keyword(Keyword::Eq)?;


    fn inner_let_declaration(state: &mut State, function_name: FunctionName, type_parameters: Vec<Variable>) -> Result<LetDeclaration> {
        let typed_term = typed_term(state)?;
        Ok(LetDeclaration { name: function_name, type_parameters, body: typed_term })
    }
    
    if state.commit_if_next_token_forall()? {
        // forall encountered
        state.request_token(Request::OpenCurly)?;
        let type_parameters = parameter_non_empty_sequence(state)?;
        state.request_token(Request::BindingSeparator)?;

        let declaration = inner_let_declaration(state, name, type_parameters)?;

        state.request_token(Request::CloseCurly)?;
        Ok(declaration)
    } else {
        // no forall
        inner_let_declaration(state, name, vec![])
    }
}

fn typed_term(state: &mut State) -> Result<TypedTerm> {
    let type_ = type_annotation(state)?;
    let term = term(state)?;
    Ok(TypedTerm { type_ , term })
}

// Parses
//    # type :
// where `:` can be optional when newline is present.
// TODO: Consider having the `:` completely optional for function declarations.
fn function_type_annotation(state: &mut State) -> Result<FunctionType> {
    state.request_keyword(Keyword::TypeAnnotationStart)?;
    let type_ = function_type(state)?;
    // TODO: Consume whitespace that's not a newline until you reach a newline or ':'
    state.request_keyword(Keyword::TypeAnnotationSeparator)?;
    Ok(type_)
}

fn type_annotation(state: &mut State) -> Result<Type> {
    state.request_keyword(Keyword::TypeAnnotationStart)?;
    let type_ = type_(state)?;
    // TODO: Consume whitespace that's not a newline until you reach a newline or ':'
    state.request_keyword(Keyword::TypeAnnotationSeparator)?;
    Ok(type_)
}


// ===Terms===
fn term(state: &mut State) -> Result<Term> {
    match start_term(state)? {
        StartTerm::VariableUse(variable) => Ok(Term::VariableUse(variable)),
        StartTerm::FunctionApplication(function_name) => {
            state.request_token(Request::OpenParen)?;
            let args = term_possibly_empty_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::FunctionApplication(function_name, args))
        },
        StartTerm::ConstructorConstant(constructor_name) => Ok(Term::ConstructorUse(constructor_name, vec![])),
        StartTerm::ConstructorApplication(constructor_name) => {
            state.request_token(Request::OpenParen)?;
            let args = term_nonempty_sequence(state)?;
            state.request_token(Request::CloseParen)?;
            Ok(Term::ConstructorUse(constructor_name, args))
        },
        StartTerm::Match => {
            let arg = term(state)?;
            let branches = pattern_branches(state)?;
            Ok(Term::Match(Box::new(arg), branches))
        },
        StartTerm::Fold => {
            let arg = term(state)?;
            let branches = pattern_branches(state)?;
            Ok(Term::Match(Box::new(arg), branches))
        },
        StartTerm::Let => {
            state.request_token(Request::OpenCurly)?;
            let bindings = nonempty_var_binding_sequence(state)?;
            state.request_token(Request::BindingSeparator)?;
            let body = term(state)?;
            state.request_token(Request::CloseCurly)?;
            Ok(Term::Let(bindings, Box::new(body)))
        },
        StartTerm::Lambda => {
            todo!()
        },
        StartTerm::Apply => {
            todo!()
        },
    }
}

// Parses    x = expr
fn var_binding(state: &mut State) -> Result<(Variable, Term)> {
    let var = variable(state)?;
    state.request_keyword(Keyword::Eq)?;
    let term = term(state)?;
    Ok((var, term))
}

fn nonempty_var_binding_sequence(state: &mut State) -> Result<Vec<(Variable, Term)>> {
    state.consume_optional_comma()?;
    delimited_nonempty_sequence_to_vector(state, var_binding, comma)
}

fn term_possibly_empty_sequence(state: &mut State) -> Result<Vec<Term>> {
    delimited_possibly_empty_sequence_to_vector(state, term, comma)
}

fn term_nonempty_sequence(state: &mut State) -> Result<Vec<Term>> {
    delimited_nonempty_sequence_to_vector(state, term, comma)
}

// ===Patterns===
impl Pattern {
    // Collect all the variables in the pattern
    fn variables(&self) -> Vec<Variable> {
        let mut vars: Vec<Variable> = vec![];

        fn pattern_loop(pattern: &Pattern, vars: &mut Vec<Variable>) {
            use Pattern::*;
            match pattern {
                Constructor(_, patterns) => {
                    for pattern in patterns {
                        pattern_loop(pattern, vars)
                    }
                },
                Variable(variable) => {
                    vars.push(variable.clone())
                },
                Anything(_) => {},
            }
        }
        pattern_loop(self, &mut vars);

        vars
    }
}

// Parses
//     {   pattern_branch0 | pattern_branch1 | ... }
// or  { | pattern_branch0 | pattern_branch1 | ... }
fn pattern_branches(state: &mut State) -> Result<Vec<PatternBranch>> {
    state.request_token(Request::OpenCurly)?;
    state.consume_optional_or()?;
    let branches = delimited_nonempty_sequence_to_vector( state, pattern_branch, or_separator)?;
    state.request_token(Request::CloseCurly)?;
    Ok(branches)
}

// Parses   pattern . body
fn pattern_branch(state: &mut State) -> Result<PatternBranch> {
    let pattern = pattern(state)?;
    state.request_token(Request::BindingSeparator)?;
    let body = term(state)?;

    Ok(PatternBranch { pattern, body })
}

fn pattern_sequence(state: &mut State) -> Result<Vec<Pattern>> {
    delimited_nonempty_sequence_to_vector(state, pattern, comma)
}

fn pattern(state: &mut State) -> Result<Pattern> {
    fn parser(state: &mut State) -> Result<Pattern> {
        match pattern_identifier(state)? {
            PatternIdentifier::Variable(variable) => Ok(Pattern::Variable(variable)),
            PatternIdentifier::ConstructorName(constructor_name) => {
                if state.is_next_token_open_paren()? {
                    // A constructor with multiple parameters.
                    state.request_token(Request::OpenParen)?;
                    let pattern_parameters = pattern_sequence(state)?;
                    state.request_token(Request::CloseParen)?;
                    Ok(Pattern::Constructor(constructor_name, pattern_parameters))
                } else {
                    // A constant
                    Ok(Pattern::Constructor(constructor_name, vec![]))
                }
            },
            PatternIdentifier::Anything(id) => {
                Ok(Pattern::Anything(id))
            }
        }
    }
    let pattern = parser(state)?;
    let duplicate_ids = identifier::duplicates(&pattern.variables());
    if duplicate_ids.is_empty() {
        Ok(pattern)
    } else {
        Err(Error::DuplicateVariableNames { duplicates: duplicate_ids })
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_type_0() -> Result<()> {
        let s = "Result(  Error,   x )";
        let mut state = State::new(s);

        let result = type_(&mut state);

        assert!(matches!(result, Ok(Type::TypeApplication(_, _))));
        let Ok(Type::TypeApplication(identifier, type_args)) = result else { unreachable!() };
        assert_eq!(type_args.len(), 2);

        assert_eq!(identifier.str(), "Result");
        assert!(matches!(type_args[0], Type::TypeApplication(_, _)));
        assert!(matches!(type_args[1], Type::VariableUse(_)));

        let Type::TypeApplication(ref err_identifier, ref err_type_args) = type_args[0] else { unreachable!() };
        assert_eq!(err_type_args.len(), 0);
        assert_eq!(err_identifier.str(), "Error");

        let Type::VariableUse(ref var_name) = type_args[1] else { unreachable!() };
        assert_eq!(var_name.str(), "x");

        Ok(())
    }

    #[test]
    fn test_type_1() -> Result<()> {
        let s = "Fn(A1, A2 -> A3)";
        let mut state = State::new(s);

        let result = type_(&mut state);

        assert!(matches!(result, Ok(Type::Arrow(_))));
        let Ok(Type::Arrow(fn_type)) = result else { unreachable!() };

        assert_eq!(fn_type.input_types.len(), 2);

        Ok(())
    }

    #[test]
    fn test_type_declaration_0() -> Result<()> {
        let s = "type Bool = enum { T | F }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Enum(_))));
        let Ok(TypeDeclaration::Enum(EnumDeclaration { name, type_parameters: _, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "Bool");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(), "T");
        assert_eq!(constructors[1].name.str(), "F");

        Ok(())
    }

    #[test]
    fn test_type_declaration_1() -> Result<()> {
        let s = "type SomeType = enum { | Const | Unary(a) | Binary(Foo(x, Bar), y) | Ternary(a, b, c) }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Enum(_))));

        let Ok(TypeDeclaration::Enum(EnumDeclaration { name, type_parameters: _, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "SomeType");

        assert_eq!(constructors.len(), 4);
        assert_eq!(constructors[0].name.str(), "Const");
        assert_eq!(constructors[1].name.str(), "Unary");
        assert_eq!(constructors[2].name.str(), "Binary");
        assert_eq!(constructors[3].name.str(), "Ternary");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 1);
        assert_eq!(constructors[2].parameters.len(), 2);
        assert_eq!(constructors[3].parameters.len(), 3);

        Ok(())
    }

    #[test]
    fn test_type_declaration_2() -> Result<()> {
        let s = "type Nat = ind { nat . Zero | Succ(nat) }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Ind(_))));
        let Ok(TypeDeclaration::Ind(IndDeclaration { name, type_parameters: _, recursive_type_var, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "Nat");
        assert_eq!(recursive_type_var.str(), "nat");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(), "Zero");
        assert_eq!(constructors[1].name.str(), "Succ");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 1);

        Ok(())
    }

    #[test]
    fn test_type_declaration_3() -> Result<()> {
        let s = "type List(a, ignored) = ind { list . Nil | Cons(a, list) }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Ok(TypeDeclaration::Ind(_))));
        let Ok(TypeDeclaration::Ind(IndDeclaration { name, type_parameters, recursive_type_var, constructors })) = result else { unreachable!() };
        assert_eq!(name.str(), "List");

        assert_eq!(type_parameters.len(), 2);
        assert_eq!(type_parameters[0].str(), "a");
        assert_eq!(type_parameters[1].str(), "ignored");

        assert_eq!(recursive_type_var.str(), "list");

        assert_eq!(constructors.len(), 2);
        assert_eq!(constructors[0].name.str(), "Nil");
        assert_eq!(constructors[1].name.str(), "Cons");

        assert_eq!(constructors[0].parameters.len(), 0);
        assert_eq!(constructors[1].parameters.len(), 2);

        Ok(())
    }

    #[test]
    fn test_type_declaration_4() -> Result<()> {
        let s = "type Foo(x, y, z, y, z) = enum { A | B }";
        let mut state = State::new(s);

        let result = type_declaration(&mut state);

        assert!(matches!(result, Err(Error::DuplicateVariableNames {..})));
        let Err(Error::DuplicateVariableNames { duplicates }) = result else { unreachable!() };
        assert_eq!(duplicates.len(), 2);
        assert_eq!(duplicates[0].str(), "y");
        assert_eq!(duplicates[1].str(), "z");

        Ok(())
    }

    #[test]
    fn test_function_declaration_0() -> Result<()> {
        let s = "fn square = # Nat -> Nat : { x . mul(x, x) }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let FunctionDeclaration { name, type_parameters, function } = result?;

        assert_eq!(name.str(), "square");
        assert_eq!(type_parameters.len(), 0);

        let Type::TypeApplication(ref t0, _) = function.type_.input_types[0] else { unreachable!() };
        let Type::TypeApplication(ref t, _) = function.type_.output_type else { unreachable!() };
        assert_eq!(t0.str(), "Nat");
        assert_eq!(t.str(), "Nat");

        let Term::FunctionApplication(mul, args) = function.body else { unreachable!() };
        let Term::VariableUse(ref arg0) = args[0] else { unreachable!() };
        let Term::VariableUse(ref arg1) = args[1] else { unreachable!() };


        assert_eq!(mul.str(), "mul");
        assert_eq!(arg0.str(), "x");
        assert_eq!(arg1.str(), "x");

        Ok(())
    }

    #[test]
    fn test_function_declaration_1() -> Result<()> {
        let s = "fn id = forall { a . # a -> a : { x . x } }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let FunctionDeclaration { name, type_parameters, function } = result?;

        assert_eq!(name.str(), "id");
        assert_eq!(type_parameters.len(), 1);

        let Type::VariableUse(ref t0) = function.type_.input_types[0] else { unreachable!() };
        let Type::VariableUse(ref t) = function.type_.output_type else { unreachable!() };
        assert_eq!(t0.str(), "a");
        assert_eq!(t.str(), "a");

        let Term::VariableUse(x) = function.body else { unreachable!() };
        assert_eq!(x.str(), "x");

        Ok(())
    }

    #[test]
    fn test_function_declaration_2() -> Result<()> {
        let s = "fn map = forall { a, b . # Fn(a -> b), List(a) -> List(b) : { f, xs . fold xs { Nil . Nil | Cons(x, state) . Cons(f(x), state) } } }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let FunctionDeclaration { name, type_parameters, function: _ } = result?;

        assert_eq!(name.str(), "map");
        assert_eq!(type_parameters.len(), 2);

        Ok(())
    }

    #[test]
    fn test_let_declaration_0() -> Result<()> {
        let s = "let two = # Nat : S(S(Zero))";
        let mut state = State::new(s);

        let result = let_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let LetDeclaration { name, type_parameters, body: _ } = result?;

        assert_eq!(name.str(), "two");
        assert_eq!(type_parameters.len(), 0);


        Ok(())
    }

    #[test]
    fn test_let_declaration_1() -> Result<()> {
        let s = "let nil = forall { a . # List : Nil }";
        let mut state = State::new(s);

        let result = let_declaration(&mut state);
        assert!(matches!(result, Ok(_)));
        let LetDeclaration { name, type_parameters, body: _ } = result?;

        assert_eq!(name.str(), "nil");
        assert_eq!(type_parameters.len(), 1);

        Ok(())
    }

    #[test]
    fn test_local_let_0() -> Result<()> {
        let s = "fn f = # Nat -> Nat : { x . let {, y = add(x, One), z = add(x, Two) . mul(y, z) } }";
        let mut state = State::new(s);

        let result = function_declaration(&mut state);
        assert!(matches!(result, Ok(_)));

        Ok(())
    }
}
