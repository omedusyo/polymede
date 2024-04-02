use crate::base::{
    indices::TypeIndex,
    types::GlobalType,
    memory::Limit,
};

pub struct Import {
    pub module_name: String,
    pub name: String,
    pub import_description: ImportDescription,
}

pub enum ImportDescription {
    FunctionTypeIndex(TypeIndex),
    MemoryType(Limit),
    GlobalType(GlobalType),
}
