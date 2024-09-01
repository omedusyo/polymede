use std::io;

type Result<A> = std::result::Result<A, io::Error>;

fn main() -> Result<()> {
    println!("hello, world!");

    Ok(())
}
