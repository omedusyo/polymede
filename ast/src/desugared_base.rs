use crate::base;
use crate::base::Type;
use crate::identifier::{Variable, Identifier, FunctionName, ConstructorName};

#[derive(Debug, Clone)]
pub enum Term {
    TypedTerm(Box<TypedTerm>),
    Int(i32),
    VariableUse(Variable),
    FunctionApplication(FunctionName, Vec<Type>, Vec<Term>),
    ConstructorUse(ConstructorName, Vec<Term>),
    Match(Box<Term>, Vec<PatternBranch>),
    Lambda(Box<Function>),
    LambdaApplication(Box<Term>, Vec<Term>),
    Let(Vec<(Variable, Term)>, Box<Term>),
    Pure(Box<Term>),
    AndThen(Box<Term>, Box<Function>), // Can assume this has exactly 1 parameter.
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Constructor(ConstructorName, Vec<Variable>),
    Variable(Variable),
    Int(i32),
    Anything(Identifier),
}

#[derive(Debug, Clone)]
pub struct PatternBranch {
    pub pattern: Pattern,
    pub body: Term,
}

#[derive(Debug, Clone)]
pub struct TypedTerm {
    pub type_: Type,
    pub term: Term
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Variable>,
    pub body: Term,
}

fn desugar_typed_term(typed_term: &base::TypedTerm) -> TypedTerm {
    TypedTerm { type_: typed_term.type_.clone(), term: desugar_term(&typed_term.term) }
}

fn desugar_function(function: &base::Function) -> Function {
    Function { parameters: function.parameters.clone(), body: desugar_term(&function.body) }
}

fn desugar_pattern_branch(pattern_branch: &base::PatternBranch) -> PatternBranch {
    use base::Pattern::*;
    let pattern = match &pattern_branch.pattern {
        Constructor(constructor_name, patterns) => {
            let mut desugared_patterns = Vec::with_capacity(patterns.len());
            // TODO: This will have to be replaced by proper
            //       compilation of nested patterns.
            for pattern in patterns {
                match pattern {
                    Variable(var) => { desugared_patterns.push(var.clone()) },
                    _ => todo!(),
                }
            }
            Pattern::Constructor(constructor_name.clone(), desugared_patterns)
        },
        Int(x) => Pattern::Int(x.clone()),
        Variable(var) => Pattern::Variable(var.clone()),
        Anything(identifier) => Pattern::Anything(identifier.clone()),
    };
    PatternBranch { pattern, body: desugar_term(&pattern_branch.body) }
}

fn desugar_term(term: &base::Term) -> Term {
    use base::Term::*;
    match term {
        TypedTerm(typed_term) => Term::TypedTerm(Box::new(desugar_typed_term(typed_term))),
        Int(x) => Term::Int(*x),
        VariableUse(var) => Term::VariableUse(var.clone()),
        FunctionApplication(fn_name, type_args, args) => Term::FunctionApplication(fn_name.clone(), type_args.clone(), args.into_iter().map(desugar_term).collect()),
        ConstructorUse(constructor_name, args) => Term::ConstructorUse(constructor_name.clone(), args.into_iter().map(desugar_term).collect()),
        Match(arg, pattern_branches) => Term::Match(Box::new(desugar_term(arg)), pattern_branches.into_iter().map(desugar_pattern_branch).collect()),
        // TODO: Replace fold with a call to anonymous function whose body is match that uses
        // recursion. 
        Fold(_arg, _pattern_branches) => todo!(),
        Lambda(function) => Term::Lambda(Box::new(desugar_function(function))),
        LambdaApplication(fn_term, args) => Term::LambdaApplication(Box::new(desugar_term(fn_term)), args.into_iter().map(desugar_term).collect()),
        Let(bindings, body) => Term::Let(bindings.iter().map(|(var, term)| (var.clone(), desugar_term(term))).collect(), Box::new(desugar_term(body))),
        Pure(term) => Term::Pure(Box::new(desugar_term(term))),
        Do(bindings, body) => desugar_do_expression(bindings, body),
    }
}

fn desugar_do_expression(bindings: &[base::DoBinding], body: &base::Term) -> Term {

    fn desugar_neighbouring_binds<'a>(mut bindings: &'a [base::DoBinding], let_bindings: &mut Vec<(Variable, Term)>) -> &'a [base::DoBinding] {
        use base::DoBinding::*;
        while let Bind(var, term) = &bindings[0] {
            let_bindings.push((var.clone(), desugar_term(term)));
            bindings = &bindings[1..];
        }
        bindings
    }

    if bindings.is_empty() {
        desugar_term(body)
    } else {
        use base::DoBinding::*;
        match &bindings[0] {
            ExecuteThenBind(var, term) => {
                let function = Function {
                    parameters: vec![var.clone()],
                    body: desugar_do_expression(&bindings[1..], body),
                };
                Term::AndThen(Box::new(desugar_term(term)), Box::new(function))
            },
            Bind(_, _) => {
                let mut let_bindings: Vec<(Variable, Term)> = vec![];
                // collect all neighbouring Bind(_, _) into one big let expression
                let bindings = desugar_neighbouring_binds(bindings, &mut let_bindings);
                Term::Let(let_bindings, Box::new(desugar_do_expression(bindings, body)))
            },
        }
    }
}
