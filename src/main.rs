mod parser;
mod token;
mod lexer;
mod expr;
mod base;

use expr::Expr;

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

fn main() {
    println!("Hello, world!");

    // lexer::example1();
    // lexer::example2();
    // lexer::example3();
    // lexer::example4();
    // lexer::example5();

    // parser::example0();
    parser::example1();
    parser::example2();

    example0();
}

