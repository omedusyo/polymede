use std::io;
use std::io::Write;
use std::fs;

use wasm;

type Result<A> = std::result::Result<A, io::Error>;

fn main() -> Result<()> {
    println!("hello, world!");
    
    Ok(())
}

