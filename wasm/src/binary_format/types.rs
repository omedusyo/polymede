use crate::binary_format::primitives::byte_stream::{
    byte, cvector, Byte, ByteStream, CVec, Either, Seq,
};
use crate::binary_format::primitives::encoder::Encoder;

use crate::base::{
    indices::TypeIndex,
    types::{
        BlockType, FunctionType, GlobalType, Mutability, NumType, RefType, ValueType, VecType,
    },
};

impl Encoder for FunctionType {
    type S = Seq<Byte, Seq<CVec<Byte>, CVec<Byte>>>;

    fn emit(&self) -> Self::S {
        let result_bytes = byte(0x60);

        let domain_types: CVec<Byte> = cvector(self.domain.iter().map(|t| t.emit()).collect());
        let codomain_types: CVec<Byte> = cvector(self.codomain.iter().map(|t| t.emit()).collect());
        result_bytes.seq(domain_types.seq(codomain_types))
    }
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

impl Encoder for VecType {
    type S = Byte;

    fn emit(&self) -> Self::S {
        use VecType::*;
        byte(match self {
            V128 => 0x7b,
        })
    }
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
impl Encoder for GlobalType {
    type S = Seq<<ValueType as Encoder>::S, <Mutability as Encoder>::S>;
    fn emit(&self) -> Self::S {
        self.type_.emit().seq(self.mutability.emit())
    }
}

// ==Mutability==
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
