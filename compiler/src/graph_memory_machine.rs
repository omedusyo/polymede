
pub type Variant = i32;

pub type FunctionName = usize;
pub type VarName = usize;
pub type ComponentIndex = u8; // max 256 components

#[derive(Debug)]
pub enum Term {
    Const(Variant),
    ByteArray(Vec<u8>),
    Tuple(Variant, Vec<Term>),
    // TODO: Consider having the projections as primitive functions.
    ProjectComponent(Box<Term>, ComponentIndex),
    Call(FunctionName, Vec<Term>),
    VarUse(VarName),
    Let(Box<Term>, Box<Term>), // let x = e0 in e1
    Match(Box<Term>, Vec<(Pattern, Term)>),
    Seq(Vec<Term>),
}

#[derive(Debug)]
pub enum Pattern {
    Variant(Variant),
    Always,
}

#[derive(Debug)]
pub struct Program {
    pub number_of_primitive_functions: usize,
    pub functions: Vec<Function>,
    pub main: Term,
}

#[derive(Debug)]
pub struct Function {
    pub number_of_parameters: usize,
    pub body: Term,
}

pub fn constant(v: Variant) -> Term {
    Term::Const(v)
}

pub fn byte_array(bytes: Vec<u8>) -> Term {
    Term::ByteArray(bytes)
}

pub fn tuple(v: Variant, components: Vec<Term>) -> Term {
    Term::Tuple(v, components)
}

pub fn project(tuple: Term, i: ComponentIndex) -> Term {
    Term::ProjectComponent(Box::new(tuple), i)
}

pub fn first(tuple: Term) -> Term {
    project(tuple, 0)
}

pub fn second(tuple: Term) -> Term {
    project(tuple, 1)
}

pub fn call(fn_name: FunctionName, args: Vec<Term>) -> Term {
    Term::Call(fn_name, args)
}

pub fn var(var_name: VarName) -> Term {
    Term::VarUse(var_name)
}

pub fn let_bind(e0: Term, e1: Term) -> Term {
    Term::Let(Box::new(e0), Box::new(e1))
}

pub fn pattern_match(arg: Term, branches: Vec<(Pattern, Term)>) -> Term {
    Term::Match(Box::new(arg), branches)
}

pub fn seq(terms: Vec<Term>) -> Term {
    Term::Seq(terms)
}
