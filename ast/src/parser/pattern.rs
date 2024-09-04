use crate::base::{Pattern, PatternBranch};
use crate::identifier;
use crate::parser::lex::lexer::Request;
use crate::parser::{
    base::{Error, Result, State},
    combinator::{
        delimited_nonempty_sequence_to_vector, delimited_possibly_empty_sequence_to_vector,
    },
    special::{comma, or_separator, start_pattern, StartPattern},
    term::term,
};

// Parses
//     {   pattern_branch0 | pattern_branch1 | ... }
// or  { | pattern_branch0 | pattern_branch1 | ... }
pub fn pattern_branches(state: &mut State) -> Result<Vec<PatternBranch>> {
    state.request_token(Request::OpenCurly)?;
    state.consume_optional_or()?;
    let branches =
        delimited_possibly_empty_sequence_to_vector(state, pattern_branch, or_separator)?;
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
        match start_pattern(state)? {
            StartPattern::Variable(variable) => Ok(Pattern::Variable(variable)),
            StartPattern::ConstructorConstant(constructor_name) => {
                Ok(Pattern::Constructor(constructor_name, vec![]))
            }
            StartPattern::ConstructorApplication(constructor_name) => {
                state.request_token(Request::OpenParen)?;
                let pattern_parameters = pattern_sequence(state)?;
                state.request_token(Request::CloseParen)?;
                Ok(Pattern::Constructor(constructor_name, pattern_parameters))
            }
            StartPattern::Int(x) => Ok(Pattern::Int(x)),
            StartPattern::Anything(id) => Ok(Pattern::Anything(id)),
        }
    }
    let pattern = parser(state)?;
    let duplicate_ids = identifier::duplicates(&pattern.variables());
    if duplicate_ids.is_empty() {
        Ok(pattern)
    } else {
        Err(Error::DuplicateVariableNames {
            duplicates: duplicate_ids.into_iter().map(Into::into).collect(),
        })
    }
}
