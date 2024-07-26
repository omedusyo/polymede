(module
  (import "runtime" "const" (func $const (param i32)))
  (import "runtime" "get_const" (func $get_const (result i32)))
  (import "runtime" "tuple" (func $tuple (param $variant i32) (param $count i32)))
  (import "runtime" "get_tuple_pointer" (func $get_tuple_pointer (result i32)))
  (import "runtime" "get_tuple_variant" (func $get_tuple_variant (result i32)))
  (import "runtime" "tuple_project" (func $tuple_project (param $index i32)))
  (import "runtime" "read_tag" (func $read_tag (result i32)))
  (import "runtime" "get_variant" (func $get_variant (result i32)))
  (import "runtime" "make_env" (func $make_env (param $count i32)))
  (import "runtime" "copy_and_extend_env" (func $copy_and_extend_env (param $arg_count i32)))
  (import "runtime" "var" (func $var (param $index i32)))
  (import "runtime" "drop_env" (func $drop_env))

  (func $add (call $const (i32.add (call $get_const) (call $get_const))))
  (export "i32_add" (func $add))

  (func $mul (call $const (i32.mul (call $get_const) (call $get_const))))
  (export "i32_mul" (func $mul))

  (func $eq (call $const (i32.eq (call $get_const) (call $get_const))))
  (export "i32_eq" (func $eq))

  (func $leq (call $const (i32.le_s (call $get_const) (call $get_const))))
  (export "i32_leq" (func $leq))

  (func $lt (call $const (i32.lt_s (call $get_const) (call $get_const))))
  (export "i32_lt" (func $lt))

  (func $inc (call $const (i32.add (call $get_const) (i32.const 1))))
  (export "i32_inc" (func $inc))

  (func $dec (call $const (i32.sub (call $get_const) (i32.const 1))))
  (export "i32_dec" (func $dec))
)
