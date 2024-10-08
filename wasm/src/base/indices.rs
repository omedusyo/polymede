#[derive(Debug, Copy, Clone)]
pub struct LocalIndex(pub u32);
#[derive(Debug, Copy, Clone)]
pub struct GlobalIndex(pub u32);
#[derive(Debug, Copy, Clone)]
pub struct LabelIndex(pub u32);

#[derive(Debug, Copy, Clone)]
pub struct TypeIndex(pub u32);
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct FunctionIndex(pub u32);
#[derive(Debug, Copy, Clone)]
pub struct TableIndex(pub u32);
#[derive(Debug, Copy, Clone)]
pub struct MemoryIndex(pub u32);
#[derive(Debug, Copy, Clone)]
pub struct DataIndex(pub u32);

pub trait Index {
    fn get(&self) -> u32;
}

impl Index for LocalIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for GlobalIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for LabelIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for TypeIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for FunctionIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for TableIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for MemoryIndex {
    fn get(&self) -> u32 {
        self.0
    }
}

impl Index for DataIndex {
    fn get(&self) -> u32 {
        self.0
    }
}
