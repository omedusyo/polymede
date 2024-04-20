use std::io;
use std::io::Write;
use std::fs;

mod graph_memory_machine;
mod interpreter;

use wasm;


type Result<A> = std::result::Result<A, io::Error>;

fn main() -> Result<()> {
    // let bytes = wasm::generate0();
    // let bytes = wasm::generate1();
    // let bytes = wasm::generate_factorial();
    // let bytes = wasm::generate_memory0();
    let bytes = wasm::generate_memory1();

    let mut file = fs::OpenOptions::new()
        .create(true) // To create a new file
        .write(true)
        // either use the ? operator or unwrap since it returns a Result
        .open("./tmp_wasm/test.wasm")?;

    file.write_all(&bytes)?;

    println!("{bytes:?}");
    
    Ok(())
}
