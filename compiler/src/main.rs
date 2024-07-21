mod graph_memory_machine;
mod interpreter;
mod polymede_compiler;
mod gmm_compiler;
mod show;

use std::io;
use std::fs;

use clap::Parser;
use ast::parser;

type Result<A> = std::result::Result<A, io::Error>;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    file: String,
}

fn example_compilation0() -> Vec<u8> {
    use crate::graph_memory_machine::{Program, Function, constant, tuple, project, var, call, call_closure, partial_apply, pattern_match};
    let standard_primitives = gmm_compiler::PrimitiveFunctions::standard();

    // functions
    let singleton = standard_primitives.number_of_primitives;

    // constructors
    let nil = 0;
    let cons = 1;
    let program = Program {
        number_of_primitive_functions: standard_primitives.number_of_primitives,
        functions: vec![
            // fn singleton(x) {
            //   Tuple(Cons, [x, Const(Nil)])
            // }
            Function {
                number_of_parameters: 1,
                body: {
                    let x = 0;
                    tuple(cons, vec![var(x), constant(nil)])
                }
            }
        ],
        // TODO: How to call primitive functions?
        // main: constant(666),
        // main: {
        //     let nil = 0;
        //     let cons = 1;
        //     tuple(cons, vec![constant(666), constant(nil)])
        // },
        // main: {
        //     call(standard_primitives.inc, vec![constant(666)])
        // },
        main: {
            call(singleton, vec![constant(230)])
        },
    };
    
    let module = gmm_compiler::compile(program, standard_primitives).unwrap();
    let bytes = module.bytes();
    bytes
}

fn check_program(program: &ast::base::Program) -> core::result::Result<(), Vec<ast::validation::base::ErrorWithLocation>> {
    ast::validation::type_formation::check_program(&program)?;
    ast::validation::term_formation::check_program(&program)?;
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    let str = fs::read_to_string(args.file)?;
    match parser::parse_program(&str) {
        Ok(program) => {
            let sh = parser::show::Show::new(&program.interner());
            println!("===Signature .pmd===");
            println!("{}", sh.show_program_declarations(&program));

            match check_program(&program) {
                Ok(()) => {
                    let gmm_program = polymede_compiler::compile(250, &program);
                    let s = show::show_program(&gmm_program).str();
                    println!("");
                    println!("");
                    println!("===Compiled .gmm===");
                    println!("{}", s);
                },
                Err(errors) => {
                    println!("===TYPE ERROR===");
                    for e in errors {
                        println!("{}\n", e.show(&sh))
                    }
                }
            }
        },
        Err(err) => {
            println!("====PARSING ERROR===");
            println!("{:?}", err);
        },
    }
    // TODO
    //example0();

    Ok(())
}

fn example0() {
    use graph_memory_machine::*;

    let program = {
        let add = 0;
        let another_add = 1;
        Program {
            // Assume the first primitive function is `inc`.
            number_of_primitive_functions: 1,
            functions: vec![
                // fn another_add(self, x, y) {
                //     x + y
                // }
                Function {
                    number_of_parameters: 3,
                    body: {
                        // let self_var = 0;
                        let x = 1;
                        let y = 2;
                        call(add, vec![var(x), var(y)])
                    }
                }
            ],
            main: call_closure(partial_apply(another_add, vec![constant(5)]), vec![constant(6)]),
        }
    };

    println!("is this exec?");
    let term = {
        let add = 0;
        let x = 1;
        let y = 2;
        //call(add, vec![var(x), var(y), call(add, vec![var(x)]), var(x)])
        //project(tuple(0, vec![constant(64), var(y), byte_array(vec![2, 5, 6, 1])]), 1)
        //let_bind(vec![var(x), var(y)], var(4))
        //pattern_match(
        //    tuple(0, vec![]),
        //    vec![(Pattern::Variant(0), var(x)), (Pattern::Variant(1), var(y))]
        //)
        //pattern_match(
        //    tuple(1, vec![var(x)]),
        //    vec![(Pattern::Variant(0), var(x)), (Pattern::Variant(1), var(y))]
        //)
        tuple(5, vec![
            pattern_match(
                tuple(1, vec![var(x)]),
                vec![(Pattern::Variant(0), var(x)), (Pattern::Variant(1), var(y))]
            )
        ])
    };


    let function = {
        let add = 0;
        Function {
            number_of_parameters: 3,
            body: {
                // let self_var = 0;
                let x = 1;
                let y = 2;
                call(add, vec![var(x), var(y)])
            }
        }
    };

    use show::{show_term, show_function, show_program};
    //let s = show_term(&term, 3).str();
    //let s = show_function(&function, 54).str();
    let s = show_program(&program).str();
    println!("{}", s);
}
