(module
  (import "runtime" "const" (func $const (param i32)))
  (import "runtime" "get_const" (func $get_const (result i32)))
  (import "runtime" "float32" (func $float32 (param f32)))
  (import "runtime" "get_float32" (func $get_float32 (result f32)))
  (import "runtime" "dup" (func $dup))
  (import "runtime" "tuple" (func $tuple (param i32) (param i32)))
  (import "runtime" "get_array_slice_pointer" (func $get_array_slice_pointer (result i32)))
  (import "runtime" "get_array_slice_count" (func $get_array_slice_count (result i32)))
  (import "runtime" "gc" (func $gc))
  (import "runtime" "concat_slices" (func $concat_slices))

  ;; console_log_int(x)
  (import "console" "log_int" (func $console_log_int (param i32)))
  ;; console_log_f32(x)
  (import "console" "log_f32" (func $console_log_f32 (param f32)))
  ;; console_log_two_ints(x, y)
  (import "console" "log_two_ints" (func $console_log_two_ints (param i32) (param i32)))
  ;; console_log_string(raw_pointer_to_start_of_string_bytes, byte_count)
  (import "console" "log_string" (func $console_log_string (param i32) (param i32)))

  (import "canvas" "get_width" (func $canvas_get_width_raw (result f32)))
  (import "canvas" "get_height" (func $canvas_get_height_raw (result f32)))
  ;; fill(r, g, b, a)
  (import "canvas" "fill" (func $canvas_fill_raw (param i32) (param i32) (param i32) (param f32)))
  ;; stroke(r, g, b, a)
  (import "canvas" "stroke" (func $canvas_stroke_raw (param i32) (param i32) (param i32) (param f32)))
  ;; line_width(x)
  (import "canvas" "line_width" (func $canvas_line_width_raw (param f32)))
  ;; fill_rect(x, y, w, h)
  (import "canvas" "fill_rect" (func $canvas_fill_rect_raw (param f32) (param f32) (param f32) (param f32)))

  ;; ===i32===
  (func $add (call $const (i32.add (call $get_const) (call $get_const))))
  (export "i32_add" (func $add))

  (func $mul (call $const (i32.mul (call $get_const) (call $get_const))))
  (export "i32_mul" (func $mul))

  (func $rem
    (local $x i32)
    (local $mod_by i32)
    (local.set $mod_by (call $get_const))
    (local.set $x (call $get_const))
    (call $const (i32.rem_s (local.get $mod_by) (local.get $x)))
  )
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

  ;; ===f32===
  (func $f32_add (call $float32 (f32.add (call $get_float32) (call $get_float32))))
  (export "f32_add" (func $f32_add))

  (func $f32_mul (call $float32 (f32.mul (call $get_float32) (call $get_float32))))
  (export "f32_mul" (func $f32_mul))

  (func $f32_neg (call $float32 (f32.neg (call $get_float32))))
  (export "f32_neg" (func $f32_neg))

  (func $f32_abs (call $float32 (f32.abs (call $get_float32))))
  (export "f32_abs" (func $f32_abs))

  (func $f32_sqrt (call $float32 (f32.sqrt (call $get_float32))))
  (export "f32_sqrt" (func $f32_sqrt))

  (func $f32_min (call $float32 (f32.min (call $get_float32) (call $get_float32))))
  (export "f32_min" (func $f32_min))

  (func $f32_max (call $float32 (f32.max (call $get_float32) (call $get_float32))))
  (export "f32_max" (func $f32_max))

  (func $f32_div
    (local $x f32)
    (local $y f32)
    (local.set $y (call $get_float32))
    (local.set $x (call $get_float32))
    (call $float32 (f32.div (local.get $x) (local.get $y)))
  )
  (export "f32_div" (func $f32_div))

  ;; ===String===
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
  (global $OP_CODE_PRINT_F32 i32 (i32.const 16))

  (global $OP_CODE_CANVAS_GET_WIDTH i32 (i32.const 60))
  (global $OP_CODE_CANVAS_GET_HEIGHT i32 (i32.const 61))
  (global $OP_CODE_CANVAS_FILL i32 (i32.const 62))
  (global $OP_CODE_CANVAS_STROKE i32 (i32.const 63))
  (global $OP_CODE_CANVAS_LINE_WIDTH i32 (i32.const 64))
  (global $OP_CODE_CANVAS_FILL_RECT i32 (i32.const 65))

  ;; WARNING: When adding a new system-call, don't forget the arity!

  ;; I32 -> Cmd(I32)
  (func $print_int 
    ;; We can assume that an integer is on the stack.
    ;; Replace the integer with a tuple whose variant is a system-call code,
    ;; and that has 1 component that's the integer
    (call $tuple (global.get $OP_CODE_PRINT_INT) (i32.const 1)))
  (export "print_int" (func $print_int))

  ;; F32 -> Cmd(I32)
  (func $print_f32
    (call $tuple (global.get $OP_CODE_PRINT_F32) (i32.const 1)))
  (export "print_f32" (func $print_f32))

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

  ;; ===Canvas===
  ;; -> Cmd(F32)
  (func (export "canvas_get_width")
    (call $tuple (global.get $OP_CODE_CANVAS_GET_WIDTH) (i32.const 0))
  )
  ;; -> Cmd(F32)
  (func (export "canvas_get_height")
    (call $tuple (global.get $OP_CODE_CANVAS_GET_HEIGHT) (i32.const 0))
  )
  ;; r,   g,   b,   a
  ;; I32, I32, I32, F32 -> Cmd(I32)
  (func (export "canvas_fill")
    (call $tuple (global.get $OP_CODE_CANVAS_FILL) (i32.const 4))
  )
  ;; r,   g,   b,   a
  ;; I32, I32, I32, F32 -> Cmd(I32)
  (func (export "canvas_stroke")
    (call $tuple (global.get $OP_CODE_CANVAS_STROKE) (i32.const 4))
  )
  ;; F32 -> Cmd(I32)
  (func (export "canvas_line_width")
    (call $tuple (global.get $OP_CODE_CANVAS_LINE_WIDTH) (i32.const 1))
  )
  ;; x,   y,   w,   h
  ;; F32, F32, F32, F32 -> Cmd(I32)
  (func (export "canvas_fill_rect")
    (call $tuple (global.get $OP_CODE_CANVAS_FILL_RECT) (i32.const 4))
  )

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
      (call $const (i32.const 0)))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_PRINT_STRING))
    (then
      (call $dup)
      (call $console_log_string (call $get_array_slice_pointer) (call $get_array_slice_count))
      (call $const (i32.const 0)))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_PRINT_F32))
    (then
      (call $console_log_f32 (call $get_float32))
      (call $const (i32.const 0)))
    (else

    ;; ===canvas===
    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_CANVAS_GET_WIDTH))
    (then (call $float32 (call $canvas_get_width_raw)))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_CANVAS_GET_HEIGHT))
    (then (call $float32 (call $canvas_get_height_raw)))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_CANVAS_FILL))
    (then (call $canvas_fill))
    (else

    (if (i32.eq (local.get $op_code) (global.get $OP_CODE_CANVAS_FILL_RECT))
    (then (call $canvas_fill_rect))

    (else

        unreachable))))))))))))))))
  )
  (export "perform_primitive_command" (func $perform_primitive_command))

  (func $canvas_fill
    (local $r i32)
    (local $g i32)
    (local $b i32)
    (local $a f32)

    (local.set $r (call $get_const))
    (local.set $g (call $get_const))
    (local.set $b (call $get_const))
    (local.set $a (call $get_float32))

    (call $canvas_fill_raw (local.get $r) (local.get $g) (local.get $b) (local.get $a))
    (call $const (i32.const 0))
  )

  (func $canvas_fill_rect
    (local $x f32)
    (local $y f32)
    (local $w f32)
    (local $h f32)

    (local.set $x (call $get_float32))
    (local.set $y (call $get_float32))
    (local.set $w (call $get_float32))
    (local.set $h (call $get_float32))

    (call $canvas_fill_rect_raw (local.get $x) (local.get $y) (local.get $w) (local.get $h))
    (call $const (i32.const 0))
  )
)
