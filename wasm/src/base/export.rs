use crate::base::indices::{GlobalIndex, FunctionIndex, MemoryIndex};

pub struct Export {
    pub name: String,
    pub export_description: ExportDescription,
}

pub enum ExportDescription {
    Function(FunctionIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}
