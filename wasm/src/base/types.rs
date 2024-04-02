use crate::base::indices::TypeIndex;

// ===Types===
pub struct FunctionType {
    pub domain: Vec<ValueType>,
    pub codomain: Vec<ValueType>,
}

pub enum BlockType {
    EmptyType,
    ValueType(ValueType),
    TypeIndex(TypeIndex),
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    NumType(NumType),
    VecType(VecType),
    RefType(RefType),
}

#[derive(Debug, Clone, Copy)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, Copy)]
pub enum VecType {
    V128,
}

#[derive(Debug, Clone, Copy)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

// ==Global Type==
pub struct GlobalType {
    pub type_: ValueType,
    pub mutability: Mutability,
}

// ==Mutability==
pub enum Mutability {
    Const,
    Var
}
