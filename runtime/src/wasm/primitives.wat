(module
  (import "runtime" "const" (func $const (param i32)))
  (import "runtime" "get_const" (func $get_const (result i32)))

  (func $add (call $const (i32.add (call $get_const) (call $get_const))))
  (export "i32_add" (func $add))

  (func $mul (call $const (i32.mul (call $get_const) (call $get_const))))
  (export "i32_mul" (func $mul))

  (func $rem (call $const (i32.rem_s (call $get_const) (call $get_const))))
  (export "i32_rem" (func $rem))

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
