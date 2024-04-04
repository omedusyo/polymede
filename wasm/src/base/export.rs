use crate::base::indices::{GlobalIndex, FunctionIndex, MemoryIndex};

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub export_description: ExportDescription,
}

#[derive(Debug)]
pub enum ExportDescription {
    Function(FunctionIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}
