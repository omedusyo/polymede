use crate::encoder::Encoder;

use crate::byte_stream::{ByteStream, byte, Byte, Seq, U32ToFixed40LEB128, Response};

// ==Limits==
pub enum Limit {
    MinToInfinity { min: u32 },
    MinMax { min: u32, max: u32 },
}

pub enum LimitsStream {
    MinToInfinity(Seq<Byte, U32ToFixed40LEB128>),
    MinMax(Seq<Byte, Seq<U32ToFixed40LEB128, U32ToFixed40LEB128>>),
}

impl ByteStream for LimitsStream {
    fn next(&mut self) -> Response {
        match self {
            Self::MinToInfinity(s) => s.next(),
            Self::MinMax(s) => s.next(),
        }
    }
}

impl Encoder for Limit {
    type S = LimitsStream;
    fn emit(&self) -> Self::S {
        use Limit::*;
        match self {
            MinToInfinity { min } => LimitsStream::MinToInfinity(byte(0x00).seq(U32ToFixed40LEB128::new(*min))),
            MinMax { min, max } => LimitsStream::MinMax(byte(0x01).seq(U32ToFixed40LEB128::new(*min).seq(U32ToFixed40LEB128::new(*max)))),
        }
    }
}
