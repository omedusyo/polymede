use crate::encoder::Encoder;
use crate::byte_stream::{string, UTF8, ByteStream, byte, Byte, Seq, Response};

use crate::indices::{TypeIndex, IndexStream};
use crate::types::GlobalType;
use crate::memory::{Limit, LimitsStream};

pub struct Import {
    pub module_name: String,
    pub name: String,
    pub import_description: ImportDescription,
}

impl Encoder for Import {
    type S = Seq<UTF8, Seq<UTF8, <ImportDescription as Encoder>::S>>;

    fn emit(&self) -> Self::S {
        string(&self.module_name).seq(string(&self.name).seq(self.import_description.emit()))
    }
}

pub enum ImportDescription {
    FunctionTypeIndex(TypeIndex),
    MemoryType(Limit),
    GlobalType(GlobalType),
}

pub enum ImportDescriptionStream {
    TypeIndex(Seq<Byte, IndexStream>),
    MemoryType(Seq<Byte, LimitsStream>),
    GlobalType(Seq<Byte, <GlobalType as Encoder>::S>),
}

impl ByteStream for ImportDescriptionStream {
    fn next(&mut self) -> Response {
        match self {
            Self::TypeIndex(s) => s.next(),
            Self::MemoryType(s) => s.next(),
            Self::GlobalType(s) => s.next(),
        }
    }
}

impl Encoder for ImportDescription {
    type S = ImportDescriptionStream;

    fn emit(&self) -> Self::S {
        match self {
            Self::FunctionTypeIndex(type_index) => ImportDescriptionStream::TypeIndex(byte(0x00).seq(type_index.emit())),
            Self::MemoryType(limits) => ImportDescriptionStream::MemoryType(byte(0x02).seq(limits.emit())),
            Self::GlobalType(global_type) => ImportDescriptionStream::GlobalType(byte(0x03).seq(global_type.emit())),
        }
    }
}
