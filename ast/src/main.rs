mod parser;

use std::io;
use std::io::Write;
use std::fs;
use clap::Parser;


// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    file: String,
}

type Result<A> = std::result::Result<A, io::Error>;

fn main() -> Result<()> {
    let args = Args::parse();
    //let mut file = fs::OpenOptions::new()
    //    .read(true)
    //    .open(args.file)?;

    let str = fs::read_to_string(args.file)?;
    match parser::parse_program(&str) {
        Ok(program) => {
            println!("{}", parser::show::show_program_declarations(program));
            //println!("{:?}", program);
        },
        Err(err) => {
            println!("{:?}", err);
        },
    }

    Ok(())
}
