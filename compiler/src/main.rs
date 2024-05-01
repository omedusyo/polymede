use std::io;
use std::io::Write;
use std::fs;

mod graph_memory_machine;
mod interpreter;
mod gmm_compiler;

use wasm;


type Result<A> = std::result::Result<A, io::Error>;

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

fn main() -> Result<()> {
    // let bytes = wasm::generate0();
    // let bytes = wasm::generate1();
    // let bytes = wasm::generate_factorial();
    // let bytes = wasm::generate_memory0();
    // let bytes = wasm::generate_memory1();

    let bytes = example_compilation0();

    let mut file = fs::OpenOptions::new()
        .create(true) // To create a new file
        .write(true)
        // either use the ? operator or unwrap since it returns a Result
        .open("./tmp_wasm/out00.wasm")?;

    file.write_all(&bytes)?;

    println!("{bytes:?}");
    
    Ok(())
}
