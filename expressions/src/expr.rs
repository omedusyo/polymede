use crate::lexer::Position;

#[derive(Debug)]
pub enum Expr {
    Nat32(Position, u32),
    Add(Position, Box<Expr>, Box<Expr>),
    Mul(Position, Box<Expr>, Box<Expr>),
}

// e = Mul(Add(Int(3), Int(2)), Int(5))
//
// push 3
// push 2
// add
// push 5
// mul
#[derive(Debug)]
pub enum Instruction {
    Push(u32),
    Add,
    Mul,
}

pub type InstructionPointer = usize;

#[derive(Debug)]
pub struct State {
    program: Vec<Instruction>,
    instruction_pointer: InstructionPointer,
    stack: Vec<u32>,
}

#[derive(Debug)]
pub enum RuntimeError {
    InvalidInstructionPointer(InstructionPointer),
    PoppingEmptyStack,
}

impl Expr {
    //pub fn parse<'a>(str: &'a str) -> Result<(&'a str, Self), ParseError> {
    //    // 1  ~> Int(1)
    //    // 1 + 1 ~> Add(Int(1), Int(1))
    //    // (1 + 1)*2 ~> Mul(Int(1), Int(1))
    //    // 1 + 1*2 ~> Add(Int(1), Mul(Int(1), Int(2)))
    //    //
    //    // 1 + 1 + 1 ~> Add(Add(Int(1), Int(1)), Int(1))
    //    todo!()
    //}

    pub fn show(&self) -> String {
        use Expr::*;
        match self {
            Nat32(_, x) => x.to_string(),
            Add(_, e0, e1) => format!("(+ {} {})", e0.show(), e1.show()),
            Mul(_, e0, e1) => format!("(* {} {})", e0.show(), e1.show()),
        }
    }

    pub fn eval(&self) -> u32 {
        use Expr::*;
        match self {
            Nat32(_, x) => *x,
            Add(_, e0, e1) => e0.eval() + e1.eval(),
            Mul(_, e0, e1) => e0.eval() * e1.eval(),
        }
    }

    pub fn compile(&self) -> Vec<Instruction> {
        match self {
            Expr::Nat32(_, x) => {
                vec![Instruction::Push(*x)]
            }
            Expr::Add(_, e0, e1) => {
                let mut instructions0 = e0.compile();
                let instructions1 = e1.compile();
                instructions0.extend(instructions1);
                instructions0.push(Instruction::Add);
                instructions0
            }
            Expr::Mul(_, e0, e1) => {
                let mut instructions0 = e0.compile();
                let instructions1 = e1.compile();
                instructions0.extend(instructions1);
                instructions0.push(Instruction::Mul);
                instructions0
            }
        }
    }
}

impl State {
    pub fn new(program: Vec<Instruction>) -> Self {
        Self {
            program,
            instruction_pointer: 0,
            stack: vec![],
        }
    }

    // struct State {
    //     program: Vec<Instruction>,
    //     instruction_pointer: InstructionPointer,
    //     stack: Vec<i32>,
    // }
    pub fn step(self) {}
}
