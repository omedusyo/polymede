
type Variant = i32;

type FunctionName = usize;
type VarName = usize;
type ComponentSize = u8; // max 256 components

enum Term {
    Const(Variant),
    ByteArray(Vec<u8>),
    Tuple(Variant, Vec<Term>),
    ProjectComponent(Box<Term>, ComponentSize),
    Call(FunctionName, Vec<Term>),
    VarUse(VarName),
    Let(Box<Term>, Box<Term>), // let x = e0 in e1
    Match(Box<Term>, Vec<(Pattern, Term)>),
    Seq(Vec<Term>),
}

enum Pattern {
    Const(Variant),
    Tuple(Variant),
    Always,
}

struct Program {
    number_of_primitive_functions: usize,
    functions: Vec<Function>,
    main: Option<Term>,
}

struct Function {
    number_of_parameters: usize,
    body: Term,
}

fn constant(v: Variant) -> Term {
    Term::Const(v)
}

fn byte_array(bytes: Vec<u8>) -> Term {
    Term::ByteArray(bytes)
}

fn tuple(v: Variant, components: Vec<Term>) -> Term {
    Term::Tuple(v, components)
}

fn project(tuple: Term, i: ComponentSize) -> Term {
    Term::ProjectComponent(Box::new(tuple), i)
}

fn first(tuple: Term) -> Term {
    project(tuple, 0)
}

fn second(tuple: Term) -> Term {
    project(tuple, 1)
}

fn call(fn_name: FunctionName, args: Vec<Term>) -> Term {
    Term::Call(fn_name, args)
}

fn var(var_name: VarName) -> Term {
    Term::VarUse(var_name)
}

fn let_bind(e0: Term, e1: Term) -> Term {
    Term::Let(Box::new(e0), Box::new(e1))
}

fn pattern_match(arg: Term, branches: Vec<(Pattern, Term)>) -> Term {
    Term::Match(Box::new(arg), branches)
}

fn seq(terms: Vec<Term>) -> Term {
    Term::Seq(terms)
}


mod tests {
    use super::*;

    #[test]
    fn test_0() {
        // functions
        let dec = 0;
        let range_loop = 1;
        let range = 2;

        // constructors
        let nil = 0;
        let cons = 1;

        let program = Program {
            number_of_primitive_functions: 1,
            // assume the first function is `dec`
            functions: vec![
                // fn rangeLoop(n, xs) {
                //  match n {
                //  | Const(nil) -> xs
                //  | _ ->
                //      let m = call dec(n)
                //      call rangeLoop(m, Tuple(cons, [m, xs]))
                //  }
                // }
                Function {
                    number_of_parameters: 2,
                    body: {
                        let n = 0;
                        let m = 1;
                        let xs = 1;
                        pattern_match(var(n), vec![
                            (Pattern::Const(nil), var(xs)),
                            (Pattern::Always, 
                                let_bind(
                                    call(dec, vec![var(n)]),
                                    call(range_loop, vec![var(m), var(xs)])
                                )
                            )
                        ])
                    },
                },
                Function {
                    number_of_parameters: 1,
                    body: {
                        let n = 0;
                        call(range_loop, vec![var(n), constant(nil)])
                    },
                }
            ],
            main: Some(call(range, vec![constant(1)])),
        };
    }
}
