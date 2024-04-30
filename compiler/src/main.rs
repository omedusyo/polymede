use std::io;
use std::io::Write;
use std::fs;

mod graph_memory_machine;
mod interpreter;
mod gmm_compiler;

use wasm;


type Result<A> = std::result::Result<A, io::Error>;

fn example_compilation0() -> Vec<u8> {
    use crate::graph_memory_machine::{Program, constant, tuple, project, var, call, call_closure, partial_apply, pattern_match};
    // functions
    let inc = 0;
    let program = Program {
        // Assume the first primitive function is `inc`.
        number_of_primitive_functions: 1,
        functions: vec![],
        // main: call(inc, vec![constant(666)]),
        main: call(inc, vec![call(inc, vec![constant(666)])]),
    };
    
    let module = gmm_compiler::compile(program).unwrap();
    let bytes = module.bytes();
    bytes
}

fn main() -> Result<()> {
    let bytes = wasm::generate0();
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
