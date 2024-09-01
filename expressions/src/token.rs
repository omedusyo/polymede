#[derive(Debug, Clone, Copy)]
pub enum Token {
    Nat32(u32),
    OpenParen,
    CloseParen,
    Operator(OperatorSymbol),
    End,
}

#[derive(Debug, Clone, Copy)]
pub enum OperatorSymbol {
    Add,
    Mul,
}
