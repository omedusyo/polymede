use std::io;
use std::io::Write;
use std::fs;

mod base;
mod lexer;
mod parser;
mod token;


type Result<A> = std::result::Result<A, io::Error>;

fn main() -> Result<()> {
    println!("hello");
    
    Ok(())
}
