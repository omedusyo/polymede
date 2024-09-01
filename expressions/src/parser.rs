use crate::expr::Expr;
use crate::lexer;
use crate::lexer::LocatedToken;
use crate::token::{OperatorSymbol, Token};

pub fn parse(str: &str) -> Result<Expr, Error> {
    let mut state = State::new(str);
    let result = start(&mut state);
    result
}

// Note that this is not a closure type. It's simply a function pointer.
type Parser<A> = fn(&mut State) -> Result<A, Error>;

#[derive(Debug)]
pub enum Error {
    LexError(lexer::Error),
}

#[derive(Debug)]
pub struct State<'a> {
    lexer_state: lexer::State<'a>,
}

impl<'state> State<'state> {
    pub fn new<'a: 'state>(str: &'a str) -> Self {
        Self {
            lexer_state: lexer::State::new(str),
        }
    }

    pub fn request_token(&mut self, request: lexer::Request) -> Result<LocatedToken, Error> {
        self.lexer_state.request(request).map_err(Error::LexError)
    }

    pub fn clone(&self) -> State<'state> {
        Self {
            lexer_state: self.lexer_state.clone(),
        }
    }
}

// ===Parser Combinator===
// left associative fold
fn left_fold_sequence<A, D>(
    state: &mut State,
    p: Parser<A>,
    delim: Parser<D>,
    combine: fn(A, D, A) -> A,
) -> Result<A, Error> {
    let mut root_a = p(state)?;

    loop {
        let saved_state = state.clone();
        match delim(state) {
            Ok(d) => {
                let a = p(state)?;
                root_a = combine(root_a, d, a);
            }
            Err(_) => {
                // backtrack.
                *state = saved_state;
                break;
            }
        }
    }
    Ok(root_a)
}

// start with a parser for an expression, then check that we have eof.
fn start(state: &mut State) -> Result<Expr, Error> {
    let root_expr = expr(state)?;
    state.request_token(lexer::Request::End)?;
    Ok(root_expr)
}

// expression:
//   an expression is an additive expression
//   additive expression is a sequence of multiplicative expressions separated by `+`
//   multiplicative expression is a sequence of base expressions
//   base expression is
//   | number literal
//   | parenthesized expression
fn expr(state: &mut State) -> Result<Expr, Error> {
    additive_expr(state)
}

// mul_exp0 `+` mul_exp1 `+` ... `+` mul_expn
fn additive_expr(state: &mut State) -> Result<Expr, Error> {
    // Note that these are all non-capturing "closures", so they will be converted to simple
    // function pointers at runtime.
    left_fold_sequence(
        state,
        multiplicative_expr,
        |state: &mut State| {
            let LocatedToken { position, .. } =
                state.request_token(lexer::Request::Operator(OperatorSymbol::Add))?;
            Ok(position)
        },
        |root: Expr, position: lexer::Position, expr_right: Expr| {
            Expr::Add(position, Box::new(root), Box::new(expr_right))
        },
    )
}

// base0 `*` base1 `*` ... `*` basen
fn multiplicative_expr(state: &mut State) -> Result<Expr, Error> {
    left_fold_sequence(
        state,
        literal_or_parenthesized_expr,
        |state: &mut State| {
            let LocatedToken { position, .. } =
                state.request_token(lexer::Request::Operator(OperatorSymbol::Mul))?;
            Ok(position)
        },
        |root: Expr, position: lexer::Position, expr_right: Expr| {
            Expr::Mul(position, Box::new(root), Box::new(expr_right))
        },
    )
}

fn literal_or_parenthesized_expr(state: &mut State) -> Result<Expr, Error> {
    let saved_state = state.clone();
    let Ok(_) = state.request_token(lexer::Request::OpenParen) else {
        *state = saved_state; // backtrack
                              // It must be a literal.
        return nat32_literal(state);
    };
    let expr = expr(state)?;
    state.request_token(lexer::Request::CloseParen)?;
    Ok(expr)
}

fn nat32_literal(state: &mut State) -> Result<Expr, Error> {
    let LocatedToken {
        token: Token::Nat32(x),
        position,
    } = state.request_token(lexer::Request::Nat32)?
    else {
        unreachable!()
    };
    Ok(Expr::Nat32(position, x))
}

pub fn example0() {
    let s = "3*2 + 8 + 1 * 2  * 5  ";
    let mut state = State::new(s);

    let result = start(&mut state);
    println!("Raw expr: {:?}", result);
    match result {
        Ok(expr) => {
            println!("Result of parsing {}: {:?}", s, expr.show());
        }
        Err(_) => {}
    }
}

pub fn example1() {
    let s = "(1 + 2) * (3 + 4)  ";
    let mut state = State::new(s);

    let result = start(&mut state);
    println!("Raw expr: {:?}", result);
    match result {
        Ok(expr) => {
            println!("Result of parsing {}: {:?}", s, expr.show());
        }
        Err(_) => {}
    }
}

pub fn example2() {
    let s = "((((3))))";
    let mut state = State::new(s);

    let result = start(&mut state);
    println!("Raw expr: {:?}", result);
    match result {
        Ok(expr) => {
            println!("Result of parsing {}: {:?}", s, expr.show());
        }
        Err(_) => {}
    }
}
