use crate::base::indices::TypeIndex;

// ===Types===
#[derive(Debug)]
pub struct FunctionType {
    pub domain: Vec<ValueType>,
    pub codomain: Vec<ValueType>,
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct GlobalType {
    pub type_: ValueType,
    pub mutability: Mutability,
}

// ==Mutability==
#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Const,
    Var
}
