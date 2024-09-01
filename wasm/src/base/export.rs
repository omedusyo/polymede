use crate::base::indices::{FunctionIndex, GlobalIndex, MemoryIndex, TableIndex};

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
