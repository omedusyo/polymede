use crate::lexer;
use crate::lexer::{LocatedToken, Position, Request};
use crate::token::{Token, SeparatorSymbol, Keyword};
use crate::base::{Program, PrimitiveType};

type Result<A> = std::result::Result<A, Error>;

#[derive(Debug, PartialEq)]
enum Type {
    VarUse(Identifier),
    TypeApplication(Identifier, Vec<Type>),
    Arrow(Box<FunctionType>),
}

#[derive(Debug, PartialEq)]
struct FunctionType {
    input_types: Vec<Type>,
    output_type: Type,
}

#[derive(Debug, Clone)]
struct Identifier {
    // TODO: Consider making this an interned String
    str: String,
    position: Position,
}

#[derive(Debug)]
struct ConstructorDeclaration {
    name: Identifier,
    parameters: Vec<Type>,
}

#[derive(Debug)]
struct EnumDeclaration {
    name: Identifier,
    type_parameters: Vec<Identifier>,
    constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
struct IndDeclaration {
    name: Identifier,
    type_parameters: Vec<Identifier>,
    recursive_type_var: Identifier,
    constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug)]
enum TypeDeclaration {
    Enum(EnumDeclaration),
    Ind(IndDeclaration),
    // TODO: structs
}

// Note that this is not a closure type. It's simply a function pointer.
type Parser<A> = fn(&mut State) -> Result<A>;

#[derive(Debug)]
pub enum Error {
    LexError(lexer::Error),
    ExpectedTypeConstructorOrTypeVar { received: Identifier },
    ExpectedTypeConstructor { received: Identifier },
    ExpectedTypeVar { received: Identifier },
    DuplicateVariableNames { duplicates: Vec<Identifier> },
}


#[derive(Debug)]
pub struct State<'a> {
    lexer_state: lexer::State<'a>
}

impl Identifier {
    fn new(str: String, position: Position) -> Self {
        Self { str, position }
    }

    fn str(&self) -> &str {
        &self.str
    }

    fn first_char(&self) -> char {
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

use std::hash::{Hash, Hasher};

impl Hash for Identifier {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.str.hash(state)
    }
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

    pub fn is_next_token_open_paren(&mut self) -> Result<bool> {
        self.lexer_state.is_next_token_open_paren().map_err(Error::LexError)
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

fn identifier(state: &mut State) -> Result<Identifier> {
    let LocatedToken { token: Token::Identifier(str), position } = state.request_token(Request::Identifier)? else { unreachable!() };
    let identifier = Identifier::new(str, position);
    Ok(identifier)
}

// ===Types===
// Parses
//   x
//   Cons(T1, T2)
//   Fn(T1, T2 -> T)
// TODO: Primitive types, e.g. I32
fn type_(state: &mut State) -> Result<Type> {
    let id = identifier(state)?;
    let c = id.first_char();
    if c.is_ascii_uppercase() {
        // constructor

        if state.is_next_token_open_paren()? {
            // A type constructor 
            state.request_token(Request::OpenParen)?;
            if id.str() == "Fn" {
                let fn_type = function_type(state)?;
                state.request_token(Request::CloseParen)?;
                return Ok(Type::Arrow(Box::new(fn_type)))
            } else {
                let type_args = type_sequence(state)?;
                state.request_token(Request::CloseParen)?;
                return Ok(Type::TypeApplication(id, type_args))
            }

        } else {
            // A type constant
            return Ok(Type::TypeApplication(id, vec![]))
        }
    } else if c.is_ascii_lowercase() {
        // type-var
        return Ok(Type::VarUse(id))
    } else {
        return Err(Error::ExpectedTypeConstructorOrTypeVar { received: id })
    }
}

// Parses   T1, T2, T3, T4 non-empty
fn type_sequence(state: &mut State) -> Result<Vec<Type>> {
    delimited_nonempty_sequence_to_vector(state, type_, comma)
}

// Parser a non-empty sequence of identifiers separated by comma
//   x1, x2, x3
// Also checks that no two identifiers are equal.
fn identifier_sequence(state: &mut State) -> Result<Vec<Identifier>> {
    fn get_duplicates(identifiers: &[Identifier]) -> Vec<Identifier> {
        use std::collections::HashSet;
        let mut seen: HashSet<&Identifier> = HashSet::new();
        let mut duplicates: Vec<Identifier> = vec![];
        // TODO: You also need the index.
        for id in identifiers {
            if !seen.insert(id) {
                // id was already present.
                duplicates.push(id.clone());
            }
        }
        duplicates
    }

    let ids = delimited_nonempty_sequence_to_vector(state, identifier, comma)?;
    let duplicate_ids = get_duplicates(&ids);
    println!("Duplicates: {:?}", duplicate_ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames { duplicates: duplicate_ids })
    }
}


// Parses  T1, T2 -> T
fn function_type(state: &mut State) -> Result<FunctionType> {
    let input_types = type_sequence(state)?;
    state.request_keyword(Keyword::Arrow)?;
    let output_type = type_(state)?;
    Ok(FunctionType { input_types, output_type })
}

fn constructor_declaration(state: &mut State) -> Result<ConstructorDeclaration> {
    let id = identifier(state)?;
    if !id.first_char().is_ascii_uppercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }

    if state.is_next_token_open_paren()? {
        // Constructor with non-zero parameters
        state.request_token(Request::OpenParen)?;
        let parameter_types = type_sequence(state)?;
        state.request_token(Request::CloseParen)?;
        Ok(ConstructorDeclaration { name: id, parameters: parameter_types, })
    } else {
        // Constant
        Ok(ConstructorDeclaration { name: id, parameters: vec![] })
    }
}

fn constructor_declaration_sequence(state: &mut State) -> Result<Vec<ConstructorDeclaration>> {
    state.consume_optional_or()?;
    delimited_nonempty_sequence_to_vector( state, constructor_declaration, or_separator)
}


fn type_declaration(state: &mut State) -> Result<TypeDeclaration> {
    state.request_keyword(Keyword::Type_)?;

    let id = identifier(state)?;
    if !id.first_char().is_ascii_uppercase() { return Err(Error::ExpectedTypeConstructor { received: id }) }

    let type_parameters = if state.is_next_token_open_paren()? {
        state.request_token(Request::OpenParen)?;
        let params = identifier_sequence(state)?;
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
                name: id,
                type_parameters,
                constructors: constructor_declaration_sequence(state)?
            })
        },
        Keyword::Ind => {
            let recursive_type_var = identifier(state)?;
            if !recursive_type_var.first_char().is_ascii_lowercase() { return Err(Error::ExpectedTypeVar { received: recursive_type_var }) }

            state.request_token(Request::BindingSeparator)?;

            TypeDeclaration::Ind(IndDeclaration {
                name: id,
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
        assert!(matches!(type_args[1], Type::VarUse(_)));

        let Type::TypeApplication(ref err_identifier, ref err_type_args) = type_args[0] else { unreachable!() };
        assert_eq!(err_type_args.len(), 0);
        assert_eq!(err_identifier.str(), "Error");

        let Type::VarUse(ref var_name) = type_args[1] else { unreachable!() };
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
        let Ok(TypeDeclaration::Enum(EnumDeclaration { name, type_parameters, constructors })) = result else { unreachable!() };
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

        let Ok(TypeDeclaration::Enum(EnumDeclaration { name, type_parameters, constructors })) = result else { unreachable!() };
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
        let Ok(TypeDeclaration::Ind(IndDeclaration { name, type_parameters, recursive_type_var, constructors })) = result else { unreachable!() };
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
        assert_eq!(duplicates[0].str, "y");
        assert_eq!(duplicates[1].str, "z");

        Ok(())
    }
}
