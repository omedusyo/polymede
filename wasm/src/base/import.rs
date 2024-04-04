use crate::base::{
    indices::TypeIndex,
    types::GlobalType,
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
    MemoryType(Limit),
    GlobalType(GlobalType),
}
