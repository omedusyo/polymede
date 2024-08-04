use crate::gmm_compiler::import_runtime_function;
use wasm::{
    syntax::{Module, fn_type, TYPE_I32},
    base::{
        indices::FunctionIndex,
        types::FunctionType,
    },
};

#[derive(Debug)]
pub struct Runtime {
    pub number_of_runtime_functions: usize,
    pub const_: FunctionIndex,
    pub get_const: FunctionIndex,
    pub tuple: FunctionIndex,
    pub get_tuple_pointer: FunctionIndex,
    pub get_tuple_variant: FunctionIndex,
    pub tuple_project: FunctionIndex,
    pub read_tag: FunctionIndex,
    pub get_variant: FunctionIndex,
    pub make_env: FunctionIndex,
    pub make_env_from: FunctionIndex,
    pub copy_and_extend_env: FunctionIndex,
    pub extend_env: FunctionIndex,
    pub var: FunctionIndex,
    pub drop_env: FunctionIndex,
    pub drop_env_then_shift: FunctionIndex,
    pub partial_apply: FunctionIndex,
    pub make_env_from_closure: FunctionIndex,
    pub pure: FunctionIndex,
    pub and_then: FunctionIndex,
    pub array_slice: FunctionIndex,
}

impl Runtime {
    pub const BYTE_ARRAY_HEADER_BYTE_SIZE: i32 = 6;
    pub const BYTE_ARRAY_TAG: u8 = 2;
    pub const GC_TAG_LIVE: u8 = 0;

    pub fn encode_byte_array(bytes: &[u8]) -> Vec<u8> {
        // Encoding of Byte Array is as follows
        //   | gc 1 byte | tag 1 byte | count 4 byte | byte array contents |
        let count = bytes.len() as i32;
        let mut result = Vec::with_capacity((Self::BYTE_ARRAY_HEADER_BYTE_SIZE + count) as usize);
        // Note that WASM encodes ints in little-endian byte order.
        result.extend_from_slice(&Self::GC_TAG_LIVE.to_le_bytes());  // 1 byte
        result.extend_from_slice(&Self::BYTE_ARRAY_TAG.to_le_bytes()); // 1 byte
        result.extend_from_slice(&count.to_le_bytes()); // 4 bytes
        result.extend_from_slice(bytes);
        result
    }

    pub fn import(module: &mut Module) -> Self {
        let mut number_of_runtime_functions = 0;
        fn import(module: &mut Module, fn_name: &str, type_: FunctionType, number_of_runtime_functions: &mut usize) -> FunctionIndex {
            let fn_index = import_runtime_function(module, fn_name, type_);
            *number_of_runtime_functions += 1;
            fn_index
        }

        let const_= import(module, "const", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let get_const = import(module, "get_const", fn_type(vec![], vec![TYPE_I32]) , &mut number_of_runtime_functions);
        let tuple = import(module, "tuple", fn_type(vec![TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let get_tuple_pointer = import(module, "get_tuple_pointer", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
        let get_tuple_variant = import(module, "get_tuple_variant", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
        let tuple_project = import(module, "tuple_project", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let read_tag = import(module, "read_tag", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
        let get_variant = import(module, "get_variant", fn_type(vec![], vec![TYPE_I32]), &mut number_of_runtime_functions);
        let make_env = import(module, "make_env", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let make_env_from = import(module, "make_env_from", fn_type(vec![TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let copy_and_extend_env = import(module, "copy_and_extend_env", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let extend_env = import(module, "extend_env", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let var = import(module, "var", fn_type(vec![TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let drop_env = import(module, "drop_env", fn_type(vec![], vec![]), &mut number_of_runtime_functions);
        let drop_env_then_shift = import(module, "drop_env_then_shift", fn_type(vec![], vec![]), &mut number_of_runtime_functions);
        let partial_apply = import(module, "partial_apply", fn_type(vec![TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);
        let make_env_from_closure = import(module, "make_env_from_closure", fn_type(vec![TYPE_I32], vec![TYPE_I32]), &mut number_of_runtime_functions);
        let pure = import(module, "pure", fn_type(vec![], vec![]), &mut number_of_runtime_functions);
        let and_then = import(module, "and_then", fn_type(vec![], vec![]), &mut number_of_runtime_functions);
        let array_slice = import(module, "array_slice", fn_type(vec![TYPE_I32, TYPE_I32, TYPE_I32], vec![]), &mut number_of_runtime_functions);

        Self {
            number_of_runtime_functions,
            const_,
            get_const,
            tuple,
            get_tuple_pointer,
            get_tuple_variant,
            tuple_project,
            read_tag,
            get_variant,
            make_env,
            make_env_from,
            copy_and_extend_env,
            extend_env,
            var,
            drop_env,
            drop_env_then_shift,
            partial_apply,
            make_env_from_closure,
            pure,
            and_then,
            array_slice,
        }
    }
}
