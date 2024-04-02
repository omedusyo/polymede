use crate::byte_stream::{ByteStream, byte, Byte, CVec, cvector, Either, Seq};
use crate::encoder::Encoder;

use crate::indices::TypeIndex;

// ===Types===
pub struct FunctionType {
    pub domain: Vec<ValueType>,
    pub codomain: Vec<ValueType>,
}

impl Encoder for FunctionType {
    type S = Seq<Byte, Seq<CVec<Byte>, CVec<Byte>>>;

    fn emit(&self) -> Self::S {
        let result_bytes = byte(0x60);

        let domain_types: CVec<Byte> = cvector(self.domain.iter().map(|t| t.emit()).collect());
        let codomain_types: CVec<Byte> = cvector(self.codomain.iter().map(|t| t.emit()).collect());
        result_bytes.seq(domain_types.seq(codomain_types))
    }
}

pub enum BlockType {
    EmptyType,
    ValueType(ValueType),
    TypeIndex(TypeIndex),
}

impl BlockType {
    pub const EMPTY_TYPE: u8 = 0x40;
}

impl Encoder for BlockType {
    type S = Either<Byte, <TypeIndex as Encoder>::S>;

    fn emit(&self) -> Self::S {
        use BlockType::*;
        match self {
            EmptyType => Either::Left(byte(Self::EMPTY_TYPE)),
            ValueType(t) => Either::Left(t.emit()),
            TypeIndex(type_index) => Either::Right(type_index.emit()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    NumType(NumType),
    VecType(VecType),
    RefType(RefType),
}

impl Encoder for ValueType {
    type S = Byte;

    fn emit(&self) -> Self::S {
        use ValueType::*;
        match self {
            NumType(t) => t.emit(),
            VecType(t) => t.emit(),
            RefType(t) => t.emit(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

impl Encoder for NumType {
    type S = Byte;

    fn emit(&self) -> Self::S {
        use NumType::*;
        byte(match self {
            I32 => 0x7f,
            I64 => 0x7e,
            F32 => 0x7d,
            F64 => 0x7c,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VecType {
    V128,
}

impl Encoder for VecType {
    type S = Byte;

    fn emit(&self) -> Self::S {
        use VecType::*;
        byte(match self {
            V128 => 0x7b,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

impl Encoder for RefType {
    type S = Byte;

    fn emit(&self) -> Self::S {
        use RefType::*;
        byte(match self {
            FuncRef => 0x70,
            ExternRef => 0x6F,
        })
    }
}

// ==Global Type==
pub struct GlobalType {
    pub type_: ValueType,
    pub mutability: Mutability,
}

impl Encoder for GlobalType {
    type S = Seq<<ValueType as Encoder>::S, <Mutability as Encoder>::S>;
    fn emit(&self) -> Self::S {
        self.type_.emit().seq(self.mutability.emit())
    }
}

// ==Mutability==
pub enum Mutability {
    Const,
    Var
}

impl Encoder for Mutability {
    type S = Byte;
    fn emit(&self) -> Self::S {
        use Mutability::*;
        match self {
            Const => byte(0x00),
            Var => byte(0x01),
        }
    }
}
