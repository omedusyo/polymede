#[derive(Debug, Clone)]
pub enum Token {
    Nat32(u32),
    OpenParen,
    CloseParen,
    Identifier(String),
    Arrow,
    Separator(SeparatorSymbol),
    End,
}

#[derive(Debug, Clone, Copy)]
pub enum SeparatorSymbol {
    Comma,
}
