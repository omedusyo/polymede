(module
  (import "runtime" "const" (func $const (param i32)))
  (import "runtime" "get_const" (func $get_const (result i32)))
  (import "runtime" "dup" (func $dup))
  (import "runtime" "tuple" (func $tuple (param i32) (param i32)))
  (import "runtime" "get_array_slice_pointer" (func $get_array_slice_pointer (result i32)))
  (import "runtime" "get_array_slice_count" (func $get_array_slice_count (result i32)))
  (import "runtime" "gc" (func $gc))
  (import "runtime" "concat_slices" (func $concat_slices))

  ;; console_log_int(x)
  (import "console" "log_int" (func $console_log_int (param i32)))
  ;; console_log_two_ints(x, y)
  (import "console" "log_two_ints" (func $console_log_two_ints (param i32) (param i32)))
  ;; console_log_string(raw_pointer_to_start_of_string_bytes, byte_count)
  (import "console" "log_string" (func $console_log_string (param i32) (param i32)))

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

  ;; Stirng, String -> String
  (func $string_concat
    ;; TODO: do a swap
    (call $concat_slices))
  (export "string_concat" (func $string_concat))

  (func $garbage_collection
    (call $gc)
    (call $const (i32.const 1))
  )
  (export "garbage_collection" (func $garbage_collection))

  ;; TODO: This whole thing needs to be replaced by function tables, no?
  ;; For some magical reason 13 is for printing an integer.
  ;; Note that 0 is reserved for `pure`,
  ;; and 1 is reserved for `and_then`.
  (global $OP_CODE_PRINT_INT i32 (i32.const 13))
  (global $OP_CODE_PRINT_TWO_INTS i32 (i32.const 14))
  (global $OP_CODE_PRINT_STRING i32 (i32.const 15))

  ;; WARNING: When adding a new system-call, don't forget the arity!

  ;; I32 -> Cmd(I32)
  (func $print_int 
    ;; We can assume that an integer is on the stack.
    ;; Replace the integer with a tuple whose variant is a system-call code,
    ;; and that has 1 component that's the integer
    (call $tuple (global.get $OP_CODE_PRINT_INT) (i32.const 1)))
  (export "print_int" (func $print_int))

  ;; I32, I32 -> Cmd(I32)
  (func $print_two_ints 
    ;; We can assume that an integer is on the stack.
    ;; Replace the integer with a tuple whose variant is a system-call code,
    ;; and that has 1 component that's the integer
    (call $tuple (global.get $OP_CODE_PRINT_TWO_INTS) (i32.const 2)))
  (export "print_two_ints" (func $print_two_ints))

  ;; String -> Cmd(I32)
  (func $print_string 
    ;; We can assume that there is a tagged pointer on top of the stack to a string slice.
    ;; Replace the pointer with a tuple whose variant is a system-call code,
    ;; and that has 1 component that's the integer
    (call $tuple (global.get $OP_CODE_PRINT_STRING) (i32.const 1)))
  (export "print_string" (func $print_string))

  ;; Takes in the system-code as input,
  ;; performs the side-effect, and eventually
  ;; could put the result on the stack if there is any.
  (func $perform_primitive_command (param $op_code i32)
    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_PRINT_INT))
    (then
      (call $console_log_int (call $get_const))
      ;; Always returns 0 (because we don't have a builtin Unit type, so it must return something) 
      (call $const (i32.const 0)))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_PRINT_TWO_INTS))
    (then
      (call $console_log_two_ints (call $get_const) (call $get_const))
      (call $const (i32.const 123456)))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_PRINT_STRING))
    (then
      (call $dup)
      (call $console_log_string (call $get_array_slice_pointer) (call $get_array_slice_count))
      (call $const (i32.const 123456)))
    (else

        unreachable))))))
  )
  (export "perform_primitive_command" (func $perform_primitive_command))
)
