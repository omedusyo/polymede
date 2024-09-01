use crate::parser::base::{Parser, Result, State};

// needs `p` to succeed atleast once
// parses
//   p d p d p d p d p
fn delimited_nonempty_sequence<S, A, D>(
    state: &mut State,
    mut s: S, // initial state of the computation
    p: Parser<A>,
    delim: Parser<D>,
    combine: fn(S, A) -> S,
) -> Result<S> {
    let a = p(state)?;
    s = combine(s, a);

    loop {
        let saved_state = state.clone();
        match delim(state) {
            Ok(_d) => {
                let a = p(state)?;
                s = combine(s, a);
            }
            Err(_) => {
                // backtrack.
                state.restore(saved_state);
                break;
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
    combine: fn(S, A) -> S,
) -> Result<S> {
    let a = {
        let saved_state = state.clone();
        match p(state) {
            Ok(a) => a,
            Err(_err) => {
                // backtrack
                state.restore(saved_state);
                return Ok(s);
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
            }
            Err(_err) => {
                // backtrack.
                state.restore(saved_state);
                break;
            }
        }
    }
    Ok(s)
}

pub fn delimited_nonempty_sequence_to_vector<A, D>(
    state: &mut State,
    p: Parser<A>,
    delim: Parser<D>,
) -> Result<Vec<A>> {
    delimited_nonempty_sequence(state, vec![], p, delim, |mut xs: Vec<A>, x: A| {
        xs.push(x);
        xs
    })
}

pub fn delimited_possibly_empty_sequence_to_vector<A, D>(
    state: &mut State,
    p: Parser<A>,
    delim: Parser<D>,
) -> Result<Vec<A>> {
    delimited_possibly_empty_sequence(state, vec![], p, delim, |mut xs: Vec<A>, x: A| {
        xs.push(x);
        xs
    })
}
