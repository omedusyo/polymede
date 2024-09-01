use crate::binary_format::primitives::byte_stream::U32ToFixed40LEB128;
use crate::binary_format::primitives::encoder::Encoder;

use crate::base::indices::Index;

pub type IndexStream = U32ToFixed40LEB128;

impl<I: Index> Encoder for I {
    type S = IndexStream;
    fn emit(&self) -> Self::S {
        U32ToFixed40LEB128::new(self.get())
    }
}
