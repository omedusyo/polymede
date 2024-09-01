mod gmm_compiler;
mod graph_memory_machine;
mod interpreter;
mod polymede_compiler;
mod runtime;
mod show;

use std::fs;
use std::io;
use std::io::Write;

use ast::parser;
use clap::Parser;

type Result<A> = std::result::Result<A, io::Error>;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    file: String,
    #[arg(short, long)]
    out: String,
}

fn check_program(
    program: &ast::base::Program,
) -> core::result::Result<(), Vec<ast::validation::base::ErrorInDeclaration>> {
    ast::validation::type_formation::check_program(program)?;
    ast::validation::term_formation::check_program(program)?;
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    let file_name = fs::read_to_string(args.file)?;

    // ===Parsing===
    let program = {
        match parser::parse_program(&file_name) {
            Ok(program) => program,
            Err(err) => {
                println!("====PARSING ERROR===");
                println!("{:?}", err);
                return Ok(());
            }
        }
    };

    let sh = parser::show::Show::new(program.interner());
    let interface_str = sh.show_program_declarations(&program);
    let mut out_interface_file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(format!("{}.pmdi", args.out))?;
    out_interface_file.write_all(interface_str.as_bytes())?;

    // ===Type Checking===
    if let Err(errors) = check_program(&program) {
        println!("===TYPE ERROR===");
        for e in errors {
            println!("{}\n", e.show(&sh))
        }
    }

    // ===.pmd ~> .gmm compiler===
    let gmm_program = polymede_compiler::compile(&program);
    let gmm_str = show::show_program(&gmm_program).str();

    let mut out_gmm_file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(format!("{}.gmm", args.out))?;
    out_gmm_file.write_all(gmm_str.as_bytes())?;

    // ===.gmm ~> .wasm compiler===
    let module = {
        match gmm_compiler::compile(gmm_program) {
            Ok(module) => module,
            Err(err) => {
                println!("===COMPILATION ERROR===");
                println!("{:?}", err);
                return Ok(());
            }
        }
    };
    let bytes = module.bytes();
    let mut wasm_file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(format!("{}.wasm", args.out))?;
    wasm_file.write_all(&bytes)?;

    Ok(())
}

fn example_compilation0() -> Vec<u8> {
    use crate::graph_memory_machine::{
        call, call_closure, constant, partial_apply, pattern_match, project, tuple, var, Function,
        FunctionOrImport, Program,
    };

    // functions
    let singleton = 0;

    // constructors
    let nil = 0;
    let cons = 1;
    let program = Program {
        functions: vec![
            // fn singleton(x) {
            //   Tuple(Cons, [x, Const(Nil)])
            // }
            FunctionOrImport::Fn(Function {
                number_of_parameters: 1,
                body: {
                    let x = 0;
                    tuple(cons, vec![var(x), constant(nil)])
                },
            }),
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
        main: { call(singleton, vec![constant(230)]) },
    };

    let module = gmm_compiler::compile(program).unwrap();
    module.bytes()
}

fn example0() {
    use graph_memory_machine::*;

    let program = {
        let add = 0;
        let another_add = 1;
        Program {
            functions: vec![
                FunctionOrImport::Import(FunctionImport {
                    number_of_parameters: 1,
                    external_name: "inc".to_string(),
                }),
                // fn another_add(self, x, y) {
                //     x + y
                // }
                FunctionOrImport::Fn(Function {
                    number_of_parameters: 3,
                    body: {
                        // let self_var = 0;
                        let x = 1;
                        let y = 2;
                        call(add, vec![var(x), var(y)])
                    },
                }),
            ],
            main: call_closure(
                partial_apply(another_add, vec![constant(5)]),
                vec![constant(6)],
            ),
        }
    };

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
        tuple(
            5,
            vec![pattern_match(
                tuple(1, vec![var(x)]),
                vec![(Pattern::Variant(0), var(x)), (Pattern::Variant(1), var(y))],
            )],
        )
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
            },
        }
    };

    use show::{show_function, show_program, show_term};
    //let s = show_term(&term, 3).str();
    //let s = show_function(&function, 54).str();
    let s = show_program(&program).str();
    println!("{}", s);
}
