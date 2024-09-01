use crate::binary_format::primitives::byte_stream::{
    byte, Byte, ByteStream, Response, Seq, U32ToFixed40LEB128, U32ToVariableLEB128,
};
use crate::binary_format::primitives::encoder::Encoder;

use crate::base::memory::{Limit, MemoryArgument};

// ===MemoryArgument===
impl Encoder for MemoryArgument {
    type S = Seq<U32ToVariableLEB128, U32ToVariableLEB128>;
    fn emit(&self) -> Self::S {
        U32ToVariableLEB128::new(self.align).seq(U32ToVariableLEB128::new(self.offset))
    }
}

// ===Limit===
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
            MinToInfinity { min } => {
                LimitsStream::MinToInfinity(byte(0x00).seq(U32ToFixed40LEB128::new(*min)))
            }
            MinMax { min, max } => LimitsStream::MinMax(
                byte(0x01).seq(U32ToFixed40LEB128::new(*min).seq(U32ToFixed40LEB128::new(*max))),
            ),
        }
    }
}
