use std::collections::BTreeMap;

// ===Types===
type TypeVar = usize;

enum ValueType {
    Int,
    TypeVarUse(TypeVar),
    List(Box<ValueType>),
}

enum ComputeType {
    ValueType(ValueType),
    // TODO: Not really sure about this ValueType thing.
    FnType(Box<ComputeType>, Box<ValueType>),
}

struct FunctionType {
    parameters: Vec<ComputeType>,
    result: ComputeType,
}

// ===Type Environment===
struct TypeVarMapping {
    mapping: BTreeMap<TypeVar, String>,
}

struct TypeEnvironment {
    // env
}

// ======User Defined Types=======
//
// ind Bool {
// | T
// | F
// }
//
// ind Nat {
// | Zero
// | S Nat
// }
//
// type Nat = Ind { L .
//
// ind List[A] {
// | Nil
// | Cons(A, List[A])
// }
//
// type List = Ind { L .
// | Nil
// | Cons(A, L)
// }

// ======Terms=========

enum ValueTerms {
    Int(i32),
    Nil,
    Cons(ValueType, Box<ValueTerms>, Box<ValueTerms>),
}
