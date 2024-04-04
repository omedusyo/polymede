#[derive(Debug, Copy, Clone)]
pub struct MemoryArgument {
    pub align: u32,
    pub offset: u32,
}

// ==Limits==
#[derive(Debug, Copy, Clone)]
pub enum Limit {
    MinToInfinity { min: u32 },
    MinMax { min: u32, max: u32 },
}
