use crate::binary_format::primitives::byte_stream::ByteStream;

pub trait Encoder {
    type S: ByteStream;
    fn emit(&self) -> Self::S;
}
