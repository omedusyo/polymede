use crate::binary_format::primitives::byte_stream::{ByteStream, Response, EVec, evector, byte, Byte, Seq, I64ToSignedLEB128, I32ToSignedLEB128};
use crate::binary_format::primitives::encoder::Encoder;
use crate::binary_format::indices::IndexStream;

use crate::base::{
    indices::Index,
    types::BlockType,
    instructions::Instruction,
};

impl Instruction {
    // control
    pub const UNREACHABLE: u8 = 0x00;
    pub const NOP: u8 = 0x01;
    pub const BLOCK: u8 = 0x02;
    pub const LOOP: u8 = 0x03;
    pub const IF: u8 = 0x04;
    pub const ELSE: u8 = 0x05;

    pub const BR: u8 = 0x0c;
    pub const BR_IF: u8 = 0x0d;

    pub const RETURN: u8 = 0x0f;
    pub const CALL: u8 = 0x10;

    pub const END: u8 = 0x0B;

    // var
    pub const LOCAL_GET: u8 = 0x20;
    pub const LOCAL_SET: u8 = 0x21;
    pub const LOCAL_TEE: u8 = 0x22;
    pub const GLOBAL_GET: u8 = 0x23;
    pub const GLOBAL_SET: u8 = 0x24;

    // i32
    pub const I32_CONST: u8 = 0x41;

    pub const I32_EQZ: u8 = 0x45;
    pub const I32_EQ: u8 = 0x46;
    pub const I32_NE: u8 = 0x47;
    pub const I32_LT_S: u8 = 0x48;
    pub const I32_LT_U: u8 = 0x49;
    pub const I32_GT_S: u8 = 0x4a;
    pub const I32_GT_U: u8 = 0x4b;
    pub const I32_LE_S: u8 = 0x4c;
    pub const I32_LE_U: u8 = 0x4d;
    pub const I32_GE_S: u8 = 0x4e;
    pub const I32_GE_U: u8 = 0x4f;

    pub const I32_CLZ: u8 = 0x67;
    pub const I32_CTZ: u8 = 0x68;
    pub const I32_POPCNT: u8 = 0x69;
    pub const I32_ADD: u8 = 0x6a;
    pub const I32_SUB: u8 = 0x6b;
    pub const I32_MUL: u8 = 0x6c;
    pub const I32_DIV_S: u8 = 0x6d;
    pub const I32_DIV_U: u8 = 0x6e;
    pub const I32_REM_S: u8 = 0x6f;
    pub const I32_REM_U: u8 = 0x70;
    pub const I32_AND: u8 = 0x71;
    pub const I32_OR: u8 = 0x72;
    pub const I32_XOR: u8 = 0x73;
    pub const I32_SHL: u8 = 0x74;
    pub const I32_SHR_S: u8 = 0x75;
    pub const I32_SHR_U: u8 = 0x76;
    pub const I32_ROTL: u8 = 0x77;
    pub const I32_ROTR: u8 = 0x78;


    // i64
    pub const I64_CONST: u8 = 0x42;
    pub const I64_ADD: u8 = 0x7c;

}

pub enum InstructionStream {
    Simple(Byte),
    ConstI32(Seq<Byte, I32ToSignedLEB128>),
    ConstI64(Seq<Byte, I64ToSignedLEB128>),
    SimpleWithIndex(Seq<Byte, IndexStream>),
    // Not just a block instruction, but also if-then, loop
    BlockExpr(Seq<
        Seq<Byte, <BlockType as Encoder>::S>,
        EVec<InstructionStream, Byte>
    >),
    IfThenElseExpr(Seq<
        Seq<Byte, <BlockType as Encoder>::S>,
        Seq<EVec<InstructionStream, Byte>, EVec<InstructionStream, Byte>>
    >),
}

impl InstructionStream {
    fn simple(opcode: u8) -> Self {
        Self::Simple(byte(opcode))
    }

    fn i32_const(x: i32) -> Self {
        let s = byte(Instruction::I32_CONST).seq(I32ToSignedLEB128::new(x));
        Self::ConstI32(s)
    }

    fn i64_const(x: i64) -> Self {
        let s = byte(Instruction::I64_CONST).seq(I64ToSignedLEB128::new(x));
        Self::ConstI64(s)
    }

    fn simple_with_index<I: Index>(opcode: u8, index: I) -> Self {
        Self::SimpleWithIndex(byte(opcode).seq(index.emit()))
    }

    fn block_expr(opcode: u8, block_type: &BlockType, instructions: &[Instruction]) -> Self {
        let header = byte(opcode).seq(block_type.emit());
        let instructions = instructions.iter().map(|instruction| instruction.emit()).collect();
        let s = header.seq(evector(instructions, byte(Instruction::END)));
        Self::BlockExpr(s)
    }

    fn if_then_else_expr(block_type: &BlockType, instructions_then: &[Instruction], instructions_else: &[Instruction]) -> Self {
        let header = byte(Instruction::IF).seq(block_type.emit());
        let instructions_then = instructions_then.iter().map(|instruction| instruction.emit()).collect();
        let instructions_else = instructions_else.iter().map(|instruction| instruction.emit()).collect();
        let instructions =
            evector(instructions_then, byte(Instruction::ELSE))
                .seq(evector(instructions_else, byte(Instruction::END)));

        let s = header.seq(instructions);
        Self::IfThenElseExpr(s)
    }
}

impl ByteStream for InstructionStream {
    fn next(&mut self) -> Response {
        match self {
            Self::Simple(s) => s.next(),
            Self::ConstI32(s) => s.next(),
            Self::ConstI64(s) => s.next(),
            Self::SimpleWithIndex(s) => s.next(),
            Self::BlockExpr(s) => s.next(),
            Self::IfThenElseExpr(s) => s.next(),
        }
    }
}

impl Encoder for Instruction {
    type S = InstructionStream;

    fn emit(&self) -> Self::S {
        use Instruction::*;

        match self {
            Unreachable => InstructionStream::simple(Self::UNREACHABLE),
            Nop => InstructionStream::simple(Self::NOP),

            // ===Control Instructions===
            Block(block_type, instructions) => InstructionStream::block_expr(Instruction::BLOCK, block_type, instructions),
            Loop(block_type, instructions) => InstructionStream::block_expr(Instruction::LOOP, block_type, instructions),
            IfThen(block_type, instructions) => InstructionStream::block_expr(Instruction::IF, block_type, instructions),
            IfThenElse(block_type, instructions_then, instructions_else) => InstructionStream::if_then_else_expr(block_type, instructions_then, instructions_else),

            Br(i) => InstructionStream::simple_with_index(Self::BR, *i),
            BrIf(i) => InstructionStream::simple_with_index(Self::BR_IF, *i),
            Call(i) => InstructionStream::simple_with_index(Self::CALL, *i),
            Return => InstructionStream::simple(Self::RETURN),

            // ===Variables Instructions===
            LocalGet(i) => InstructionStream::simple_with_index(Self::LOCAL_GET, *i),
            LocalSet(i) => InstructionStream::simple_with_index(Self::LOCAL_SET, *i),
            LocalTee(i) => InstructionStream::simple_with_index(Self::LOCAL_TEE, *i),
            GlobalGet(i) => InstructionStream::simple_with_index(Self::GLOBAL_GET, *i),
            GlobalSet(i) => InstructionStream::simple_with_index(Self::GLOBAL_SET, *i),

            // ===Numeric Instructions===
            // i32
            I32Const(x) => InstructionStream::i32_const(*x),

            I32Eqz => InstructionStream::simple(Self::I32_EQZ),
            I32Eq => InstructionStream::simple(Self::I32_EQ),
            I32Ne => InstructionStream::simple(Self::I32_NE),
            I32LtS => InstructionStream::simple(Self::I32_LT_S),
            I32LtU => InstructionStream::simple(Self::I32_LT_U),
            I32GtS => InstructionStream::simple(Self::I32_GT_S),
            I32GtU => InstructionStream::simple(Self::I32_GT_U),
            I32LeS => InstructionStream::simple(Self::I32_LE_S),
            I32LeU => InstructionStream::simple(Self::I32_LE_U),
            I32GeS => InstructionStream::simple(Self::I32_GE_S),
            I32GeU => InstructionStream::simple(Self::I32_GE_U),

            I32Clz => InstructionStream::simple(Self::I32_CLZ),
            I32Ctz => InstructionStream::simple(Self::I32_CTZ),
            I32Popcnt => InstructionStream::simple(Self::I32_POPCNT),
            I32Add => InstructionStream::simple(Self::I32_ADD),
            I32Sub => InstructionStream::simple(Self::I32_SUB),
            I32Mul => InstructionStream::simple(Self::I32_MUL),
            I32DivS => InstructionStream::simple(Self::I32_DIV_S),
            I32DivU => InstructionStream::simple(Self::I32_DIV_U),
            I32RemS => InstructionStream::simple(Self::I32_REM_S),
            I32RemU => InstructionStream::simple(Self::I32_REM_U),
            I32And => InstructionStream::simple(Self::I32_AND),
            I32Or => InstructionStream::simple(Self::I32_OR),
            I32Xor => InstructionStream::simple(Self::I32_XOR),
            I32Shl => InstructionStream::simple(Self::I32_SHL),
            I32ShrS => InstructionStream::simple(Self::I32_SHR_S),
            I32ShrU => InstructionStream::simple(Self::I32_SHR_U),
            I32Rotl => InstructionStream::simple(Self::I32_ROTL),
            I32Rotr => InstructionStream::simple(Self::I32_ROTR),

            // i64
            I64Const(x) => InstructionStream::i64_const(*x),
            I64Add => InstructionStream::simple(Self::I64_ADD),

            _ => todo!(),
        }
    }
}
