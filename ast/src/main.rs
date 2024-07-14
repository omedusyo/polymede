mod parser;
mod validation;

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

fn check_program(program: &parser::base::Program) -> Result<(), validation::base::ErrorWithLocation> {
    validation::type_formation::check_program(&program)?;
    validation::term_formation::check_program(&program)?;
    Ok(())
}

fn main() -> Result<(), io::Error> {
    let args = Args::parse();
    //let mut file = fs::OpenOptions::new()
    //    .read(true)
    //    .open(args.file)?;

    let str = fs::read_to_string(args.file)?;
    match parser::parse_program(&str) {
        Ok(program) => {
            let sh = parser::show::Show::new(&program.interner());
            println!("{}", sh.show_program_declarations(&program));

            match check_program(&program) {
                Ok(_) => {},
                Err(e) => println!("{}", e.show(&sh))
            }
        },
        Err(err) => {
            println!("{:?}", err);
        },
    }

    Ok(())
}
