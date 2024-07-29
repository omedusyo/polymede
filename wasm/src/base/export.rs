use crate::base::indices::{GlobalIndex, FunctionIndex, TableIndex, MemoryIndex};

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub export_description: ExportDescription,
}

#[derive(Debug)]
pub enum ExportDescription {
    Function(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}
