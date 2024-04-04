use crate::base::{
    indices::{LocalIndex, GlobalIndex, LabelIndex, FunctionIndex},
    types::BlockType,
};

pub enum Instruction {
    Unreachable,
    Nop,
    
    // ===Control Instructions===
    Block(BlockType, Vec<Instruction>),
    Loop(BlockType, Vec<Instruction>),
    IfThen(BlockType, Vec<Instruction>),
    IfThenElse(BlockType, Vec<Instruction>, Vec<Instruction>),

    Br(LabelIndex),
    BrIf(LabelIndex),
    Call(FunctionIndex),

    Return,

    // ===Variable Instructions====
    LocalGet(LocalIndex),
    LocalSet(LocalIndex),
    LocalTee(LocalIndex),
    GlobalGet(GlobalIndex),
    GlobalSet(GlobalIndex),

    // ===Numeric Instructions===
    // i32
    I32Const(i32),

    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    // i64
    I64Const(i64),
    I64Add,
}
