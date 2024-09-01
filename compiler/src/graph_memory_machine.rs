pub type Variant = i32;

pub type FunctionName = usize;
pub type VarName = usize;
pub type ExternalName = String;
pub type ComponentIndex = u8; // max 256 components

#[derive(Debug)]
pub enum Term {
    Const(Variant),
    // There is a choice here. We could encode floats directly as Const.
    // Since we are targetting wasm, it would have to be little-endian IEEE-754 encoding.
    // But potentially we could target other VM's, that could use different encodings.
    // So it seems unnatural to encode floats directly into const here. Hence we defer the encoding
    // to actual gmm compilers.
    Float32(f32),
    ByteArray(Vec<u8>),
    Tuple(Variant, Vec<Term>),
    ProjectComponent(Box<Term>, ComponentIndex),
    Call(FunctionName, Vec<Term>),
    // This is used to created closures.
    // Note that even though the arguments are in general variables, some of these arguments
    // can be projections of variables.
    PartialApply(FunctionName, Vec<Term>),
    CallClosure(Box<Term>, Vec<Term>),
    VarUse(VarName),
    Let(Vec<Term>, Box<Term>), // let x0 = e0, x1 = e1, ... in body
    // Intention behind match is that the argument will be bound to a new variable
    // that will be accessible in the bodies.
    Match(Box<Term>, Vec<(Pattern, Term)>),
    Pure(Box<Term>),
    CommandAndThen(Box<Term>, Box<Continuation>),
    Receive,
}

#[derive(Debug)]
pub struct Continuation {
    pub body: Term,
}

#[derive(Debug)]
pub enum Pattern {
    Variant(Variant),
    Always,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionOrImport>,
    pub main: Term,
}

#[derive(Debug)]
pub enum FunctionOrImport {
    Fn(Function),
    Import(FunctionImport),
}

#[derive(Debug)]
pub struct Function {
    pub number_of_parameters: usize,
    pub body: Term,
}

#[derive(Debug)]
pub struct FunctionImport {
    pub number_of_parameters: usize,
    pub external_name: ExternalName,
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

pub fn call_closure(closure_term: Term, args: Vec<Term>) -> Term {
    Term::CallClosure(Box::new(closure_term), args)
}

pub fn partial_apply(fn_name: FunctionName, args: Vec<Term>) -> Term {
    Term::PartialApply(fn_name, args)
}

pub fn var(var_name: VarName) -> Term {
    Term::VarUse(var_name)
}

pub fn let_bind(args: Vec<Term>, e1: Term) -> Term {
    Term::Let(args, Box::new(e1))
}

pub fn pattern_match(arg: Term, branches: Vec<(Pattern, Term)>) -> Term {
    Term::Match(Box::new(arg), branches)
}
