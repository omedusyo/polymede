use crate::binary_format::indices::IndexStream;
use crate::binary_format::primitives::byte_stream::{byte, string, Byte, ByteStream, Seq, UTF8};
use crate::binary_format::primitives::encoder::Encoder;

use crate::base::export::{Export, ExportDescription};

impl Encoder for Export {
    type S = Seq<UTF8, <ExportDescription as Encoder>::S>;

    fn emit(&self) -> Self::S {
        string(&self.name).seq(self.export_description.emit())
    }
}

impl ExportDescription {
    const FUNCTION_PREFIX: u8 = 0x00;
    const TABLE_PREFIX: u8 = 0x01;
    const MEMORY_PREFIX: u8 = 0x02;
    const GLOBAL_PREFIX: u8 = 0x03;
}

impl Encoder for ExportDescription {
    type S = Seq<Byte, IndexStream>;

    fn emit(&self) -> Self::S {
        use ExportDescription::*;
        match self {
            Function(index) => byte(Self::FUNCTION_PREFIX).seq(index.emit()),
            Table(index) => byte(Self::TABLE_PREFIX).seq(index.emit()),
            Memory(index) => byte(Self::MEMORY_PREFIX).seq(index.emit()),
            Global(index) => byte(Self::GLOBAL_PREFIX).seq(index.emit()),
        }
    }
}
