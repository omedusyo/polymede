mod parser;
mod token;
mod lexer;
mod expr;
mod base;
mod codegen;

use expr::Expr;
use codegen::compile_from_str;

use std::io;
use std::io::Write;
use std::fs;

fn example0() {
    use Expr::*;
    let dummy_position = lexer::Position { column: 1, line: 1 };
    let expr = Mul(dummy_position, Box::new(Add(dummy_position, Box::new(Nat32(dummy_position, 1)), Box::new(Nat32(dummy_position, 1)))), Box::new(Nat32(dummy_position, 3)));
    let v = expr.eval();

    let c = expr.compile();

    println!("Expr value is: {}", v);

    println!("");
    println!("Expr value is: {:?}", c);
}

fn full_example0() -> Result<(), io::Error> {
    let module = compile_from_str("(1 + 1)*(1 + 1) * 6").unwrap();

    let bytes = module.bytes();

    let mut file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open("./tmp_wasm/test.wasm")?;

    file.write_all(&bytes)?;

    Ok(())
}

fn main() -> Result<(), io::Error> {
    // lexer::example1();
    // lexer::example2();
    // lexer::example3();
    // lexer::example4();
    // lexer::example5();

    // parser::example0();
    // parser::example1();
    // parser::example2();

    // example0();

    full_example0()?;

    Ok(())
}
