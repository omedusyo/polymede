use crate::parser::lex::lexer::Request;
use crate::parser::{
    base::{State, Result, Error, Pattern, PatternBranch},
    identifier,
    identifier::{Variable, ConstructorName, Identifier, identifier, variable},
    term::term,
    special::{comma, or_separator},
    combinator::{delimited_nonempty_sequence_to_vector, delimited_possibly_empty_sequence_to_vector},
};

enum PatternIdentifier {
    Variable(Variable),
    ConstructorName(ConstructorName),
    Anything(Identifier),
}

fn pattern_identifier(state: &mut State) -> Result<PatternIdentifier> {
    let id = identifier(state)?;
    let c = id.first_char(state.interner());
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
pub fn parameter_non_empty_sequence(state: &mut State) -> Result<Vec<Identifier>> {
    let ids = delimited_nonempty_sequence_to_vector(state, variable, comma)?;
    let duplicate_ids = identifier::duplicates(&ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames { duplicates: duplicate_ids })
    }
}

pub fn parameter_possibly_empty_sequence(state: &mut State) -> Result<Vec<Identifier>> {
    let ids = delimited_possibly_empty_sequence_to_vector(state, variable, comma)?;
    let duplicate_ids = identifier::duplicates(&ids);
    if duplicate_ids.is_empty() {
        Ok(ids)
    } else {
        Err(Error::DuplicateVariableNames { duplicates: duplicate_ids })
    }
}

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
pub fn pattern_branches(state: &mut State) -> Result<Vec<PatternBranch>> {
    state.request_token(Request::OpenCurly)?;
    state.consume_optional_or()?;
    let branches = delimited_possibly_empty_sequence_to_vector( state, pattern_branch, or_separator)?;
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
