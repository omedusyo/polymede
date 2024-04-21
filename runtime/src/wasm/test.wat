(module
  (import "console" "log" (func $log (param i32)))
  (import "console" "logStack" (func $log_stack))
  (import "console" "logHeap" (func $log_heap))

  (memory 1 10)
  (export "memory" (memory 0))


  (; Linear Stack := Linear Memory Stack ;)
  (global $stack_start (mut i32) (i32.const 0))
  (export "stack_start" (global $stack_start))
  (global $stack (mut i32) (i32.const 0))
  (export "stack" (global $stack))

  (; start of the heap ;)
  (global $heap (mut i32) (i32.const 1024))
  (export "heap" (global $heap))
  (; next free space on the heap ;)
  (global $free (mut i32) (i32.const 1024)) 
  (export "free" (global $free))


  (func $inc_stack (param $offset i32)
    (global.set $stack (i32.add (global.get $stack) (local.get $offset)))
  )

  (func $inc_free (param $offset i32)
    (global.set $free (i32.add (global.get $free) (local.get $offset)))
  )

  (; ===Constant=== ;)
  (; Pushes constant to Linear Stack ;)
  (func $const (param $variant i32)
    (i32.store8 (global.get $stack) (i32.const 1))  (; 1 is tag for Const ;)
    (call $inc_stack (i32.const 1))

    (i32.store (global.get $stack) (local.get $variant))
    (call $inc_stack (i32.const 4))
  )

  (; Assumes that a constant is on the top of the Linear Stack. ;)
  (; Moves it to WASM stack ;)
  (func $get_const (result i32)
    (call $inc_stack (i32.const -4))
    (i32.load (global.get $stack))
    (call $inc_stack (i32.const -1))
  )

  (; ===Tuple=== ;)
  (; Consumes $size many tagged pointers on the stack, and moves them onto the heap ;)
  (; 1 <= $size <= 255 ;)
  (func $tuple (param $variant i32) (param $size i32)
    (local $component_bytes i32)
    (local $tuple_pointer i32)

    (local.set $tuple_pointer (global.get $free))
    (; ==header== ;)
    (; 1 bit (takes the whole byte ofcourse) for garbage collection. ;)
    (i32.store8 (global.get $free) (i32.const 0))  (; 1 is tag for Const ;)
    (call $inc_free (i32.const 1))
    (; 4 bytes for variant ;)
    (i32.store (global.get $free) (local.get $variant))  (; 0 is tag for Const ;)
    (call $inc_free (i32.const 4))
    (; 1 byte for number of components ;)
    (i32.store8 (global.get $free) (local.get $size))  (; 1 is tag for Const ;)
    (call $inc_free (i32.const 1))
    
    (; ==payload== ;)
    (; go back to the start of the component on the stack ;)
    (local.set $component_bytes (i32.mul (local.get $size) (i32.const 5)))
    (call $inc_stack (i32.sub (i32.const 0) (local.get $component_bytes)))

    (memory.copy (global.get $free) (global.get $stack) (local.get $component_bytes))
    
    (; ==return address on the stack== ;)
    (i32.store8 (global.get $stack) (i32.const 3))  (; 3 is tag for Tuple ;)
    (call $inc_stack (i32.const 1))

    (i32.store (global.get $stack) (local.get $tuple_pointer))
    (call $inc_stack (i32.const 4))
  )

  (; ===Primitive Operations=== ;)
  (func $add
    (call $const (i32.add (call $get_const) (call $get_const)))
  )

  (; ===Examples=== ;)
  (func $example_stack0
    (call $const (i32.const 5))
    (call $const (i32.const 7))
    (call $log_stack)
    (call $add)
    (call $log_stack)
    (; (call $log (call $get_const)) ;)
    (; (call $log (call $get_const)) ;)
  )

  (func $example_heap0
    (call $const (i32.const 15))
    (call $const (i32.const 16))
    (call $log_stack)

    (call $tuple (i32.const 4) (i32.const 2))
    (call $log_stack)
    (call $log_heap)
  )

  (func $init
    (; (call $example_stack0) ;)
    (call $example_heap0)
  )
  (export "init" (func $init))
)
