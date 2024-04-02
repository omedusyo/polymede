// ==Limits==
pub enum Limit {
    MinToInfinity { min: u32 },
    MinMax { min: u32, max: u32 },
}
