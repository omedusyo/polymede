use crate::encoder::Encoder;
use crate::byte_stream::{string, UTF8, ByteStream, byte, Byte, Seq};

use crate::indices::{GlobalIndex, FunctionIndex, MemoryIndex, IndexStream};

pub struct Export {
    pub name: String,
    pub export_description: ExportDescription,
}

impl Encoder for Export {
    type S = Seq<UTF8, <ExportDescription as Encoder>::S>;

    fn emit(&self) -> Self::S {
        string(&self.name).seq(self.export_description.emit())
    }
}

pub enum ExportDescription {
    Function(FunctionIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}

impl ExportDescription {
    const FUNCTION_PREFIX: u8 = 0x00;
    const MEMORY_PREFIX: u8 = 0x02;
    const GLOBAL_PREFIX: u8 = 0x03;
}

impl Encoder for ExportDescription {
    type S = Seq<Byte, IndexStream>;

    fn emit(&self) -> Self::S {
        use ExportDescription::*;
        match self {
            Function(index) => byte(Self::FUNCTION_PREFIX).seq(index.emit()),
            Memory(index) => byte(Self::MEMORY_PREFIX).seq(index.emit()),
            Global(index) => byte(Self::GLOBAL_PREFIX).seq(index.emit()),
        }
    }
}
