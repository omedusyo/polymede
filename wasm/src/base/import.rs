use crate::base::{
    indices::TypeIndex,
    types::{GlobalType, RefType},
    memory::Limit,
};

#[derive(Debug)]
pub struct Import {
    pub module_name: String,
    pub name: String,
    pub import_description: ImportDescription,
}

#[derive(Debug)]
pub enum ImportDescription {
    FunctionTypeIndex(TypeIndex),
    TableType(RefType, Limit),
    MemoryType(Limit),
    GlobalType(GlobalType),
}
