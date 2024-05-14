use crate::lexer;
use crate::lexer::{LocatedToken, Position};
use crate::token::{Token, SeparatorSymbol};
use crate::base::{Program, PrimitiveType};

type Result<A> = std::result::Result<A, Error>;

#[derive(Debug)]
enum Type {
    VarUse(Identifier),
    TypeApplication(Identifier, Vec<Type>),
    Arrow(Box<FunctionType>),
}

#[derive(Debug)]
struct FunctionType {
    input_types: Vec<Type>,
    output_type: Type,
}

#[derive(Debug)]
struct Identifier {
    str: String,
    position: Position,
}

// Note that this is not a closure type. It's simply a function pointer.
type Parser<A> = fn(&mut State) -> Result<A>;

#[derive(Debug)]
pub enum Error {
    LexError(lexer::Error),
    ExpectedTypeConstructorOrTypeVar { received: Identifier },
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
}

impl <'state> State<'state> {
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self {
            lexer_state: lexer::State::new(str)
        }
    }

    pub fn request_token(&mut self, request: lexer::Request) -> Result<LocatedToken> {
        self.lexer_state.request(request).map_err(Error::LexError)
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

fn comma(state: &mut State) -> Result<Position> {
    let LocatedToken { position, .. } = state.request_token(lexer::Request::Separator(SeparatorSymbol::Comma))?;
    Ok(position)
}

// ===Types===
fn type_(state: &mut State) -> Result<Type> {
    let LocatedToken { token: Token::Identifier(str), position } = state.request_token(lexer::Request::Identifier)? else { unreachable!() };
    // identifier is guaranteed to be non-empty
    let c = str.chars().nth(0).unwrap();
    let identifier = Identifier::new(str, position);
    if c.is_ascii_uppercase() {
        // constructor
        let saved_state = state.clone();
        let Ok(_) = state.request_token(lexer::Request::OpenParen) else {
            *state = saved_state; // backtrack
            // A type constant
            return Ok(Type::TypeApplication(identifier, vec![]))
        };
        // A type constructor 
        if identifier.str() == "Fn" {
            let fn_type = function_type(state)?;
            state.request_token(lexer::Request::CloseParen)?;
            return Ok(Type::Arrow(Box::new(fn_type)))
        } else {
            let type_args = type_sequence(state)?;
            state.request_token(lexer::Request::CloseParen)?;
            return Ok(Type::TypeApplication(identifier, type_args))
        }
    } else if c.is_ascii_lowercase() {
        // type-var
        return Ok(Type::VarUse(identifier))
    } else {
        return Err(Error::ExpectedTypeConstructorOrTypeVar { received: identifier })
    }
}

fn type_sequence(state: &mut State) -> Result<Vec<Type>> {
    delimited_nonempty_sequence(
        state,
        vec![],
        type_,
        comma,
        |mut types: Vec<Type>, type_: Type| {
            types.push(type_);
            types
        }
    )
}

fn function_type(state: &mut State) -> Result<FunctionType> {
    let input_types = type_sequence(state)?;
    state.request_token(lexer::Request::Arrow)?;
    let output_type = type_(state)?;
    Ok(FunctionType { input_types, output_type })
}

mod tests {
    use super::*;

    #[test]
    fn test_type_0() -> Result<()> {
        let s = "Result(  Error,   x )";
        let mut state = State::new(s);

        let result = type_(&mut state);

        matches!(result, Ok(Type::TypeApplication(_, _)));
        let Ok(Type::TypeApplication(identifier, type_args)) = result else { unreachable!() };
        assert_eq!(type_args.len(), 2);

        assert_eq!(identifier.str(), "Result");
        matches!(type_args[0], Type::TypeApplication(_, _));
        matches!(type_args[1], Type::VarUse(_));

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

        matches!(result, Ok(Type::TypeApplication(_, _)));
        let Ok(Type::Arrow(fn_type)) = result else { unreachable!() };

        assert_eq!(fn_type.input_types.len(), 2);

        Ok(())
    }
}
