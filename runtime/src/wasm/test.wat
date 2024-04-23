(; memory.copy $destination $source $size ;)

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
  (global $frame (mut i32) (i32.const 0))
  (export "frame" (global $frame))
  (global $env (mut i32) (i32.const 0))
  (export "env" (global $env))

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

  (; ===Environment=== ;)
  (; Consumes $offset elements on the top of the stack ;)
  (; and creates an environment out of them ;)
  (func $make_env (param $offset i32)
    (; This is terrible inefficient. We're just shifting the top of the stack ;)
    (; by 4 bytes, and then writing the size as the prefix. ;)

    (local $bytes i32)
    (local $source i32)
    (local $destination i32)

    (; source := stack - 5*offset ;)
    (local.set $bytes (i32.mul (i32.const 5) (local.get $offset)))
    (local.set $source (i32.sub (global.get $stack) (local.get $bytes)))
    (local.set $destination (i32.add (local.get $source) (i32.const 4)))

    (; Copy the tagged pointers (each is 5 bytes) ;)
    (memory.copy (local.get $destination) (local.get $source) (local.get $bytes))

    (; First 4 bytes are size ;)
    (i32.store (local.get $source) (local.get $offset))
    (call $inc_stack (i32.const 4))

    (; Sets the environment on the Shadow Stack as the current environment ;)
    (global.set $env (local.get $destination))
  )

  (; Assume N arguments on the stack (here N == $arg_size) ;)
  (; The N arguments are consumed, and in their place we have ;)
  (; * Saved current env register ;)
  (; * Followed by the new extended environment with the consumed arguments ;)
  (; The env register is updated to point to this new environment. ;)
  (func $open_new_scope (param $arg_size i32)
    (; This is the number of bytes for old env + size. In current scheme this is just constant 5. ;)
    (local $header_bytes i32)

    (local $old_env_size i32)
    (local $old_env_bytes i32)

    (; start of where the current arguments will be copied to ;)
    (local $destination_arg i32)
    (local $arg_bytes i32)

    (local.set $header_bytes (i32.const 5))

    (local.set $old_env_size (i32.load (i32.sub (global.get $env) (i32.const 4))))
    (local.set $old_env_bytes (i32.mul (i32.const 5) (local.get $old_env_size)))
    (local.set $arg_bytes (i32.mul (i32.const 5) (local.get $arg_size)))

    (call $inc_stack (i32.sub (i32.const 0) (local.get $arg_bytes)))

    (; ==Copying the current arguments to the env of the new env== ;)
    (local.set $destination_arg (i32.add (global.get $stack) (i32.add (local.get $header_bytes) (local.get $old_env_bytes))))
    (memory.copy (local.get $destination_arg) (global.get $stack) (local.get $arg_bytes))

    (; ==Copying the old env== ;)
    (memory.copy (i32.add (global.get $stack) (local.get $header_bytes)) (global.get $env) (local.get $old_env_bytes))

    (; ==Saving old env register== ;)
    (i32.store (global.get $stack) (global.get $env))

    (; ==Updating the env register== ;) 
    (global.set $env (i32.add (global.get $stack) (local.get $header_bytes)))
    (; 4 bytes for the pointer to the old env which is at the start of the header ;)
    (call $inc_stack (i32.const 4))

    (; ==Prefixing the environment with the size ;)
    (; env size takes up 1 byte ;)
    (i32.store8 (global.get $stack) (i32.add (local.get $old_env_size) (local.get $arg_size)))
    (call $inc_stack (i32.const 1))

    (; ==Moving the stack== ;)
    (call $inc_stack (i32.add (local.get $old_env_bytes) (local.get $arg_bytes)))
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
    (call $inc_free (local.get $component_bytes))
    
    (; ==tagged pointer to the tuple on the stack== ;)
    (i32.store8 (global.get $stack) (i32.const 3))  (; 3 is tag for Tuple ;)
    (call $inc_stack (i32.const 1))

    (i32.store (global.get $stack) (local.get $tuple_pointer))
    (call $inc_stack (i32.const 4))
  )

  (; TODO: I need to have n-th variable in the current environment ;)
  (; copies the n-th var to the top of the current stack ;)
  (; Suddenly I need call-frames ;)
  (func $var (param i32)
  )

  (; ===Primitive Operations=== ;)
  (func $add
    (call $const (i32.add (call $get_const) (call $get_const)))
  )

  (func $inc
    (call $const (i32.add (call $get_const) (i32.const 1)))
  )

  (func $dec
    (call $const (i32.sub (call $get_const) (i32.const 1)))
  )

  (; ===Examples=== ;)
  (; ==functions== ;)
  (func $true
    (call $const (i32.const 1))
  )

  (func $false
    (call $const (i32.const 0))
  )

  (func $nil
    (call $const (i32.const 0))
  )

  (func $cons
    (call $tuple (i32.const 1) (i32.const 2))
  )

  (; fn singleton_plus_one(x) { ;)
  (;   Tuple(Cons, [call inc(x), Const(Nil)]) ;)
  (; } ;)
  (func $singleton_plus_one
    (call $inc)
    (call $nil)
    (call $cons)
  )

  (; fn is_zero(n) { ;)
  (;   match n { ;)
  (;   | 0 -> t ;)
  (;   | _ -> f ;)
  (;   } ;)
  (; } ;)
  (func $is_zero
    (call $get_const)
    (if (i32.eqz)
      (then
        (call $true)
      )
      (else
        (call $false)
      )
    )
  )

  (; fn sum(n) { ;)
  (;   match n { ;)
  (;   | 0 -> 0 ;)
  (;   | _ -> n + sum(dec(n)) ;)
  (;   } ;)
  (; } ;)
  (func $sum
    (call $get_const)
    (if (i32.eqz)
      (then
        (call $const (i32.const 0))
      )
      (else
      )
    )
  )

  (; ==tests== ;)
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

  (; cons(125, nil) ;)
  (func $example_list_0
    (call $const (i32.const 125))
    (call $nil)
    (call $cons)
    (call $log_stack)
    (call $log_heap)
  )

  (; cons(125, cons(127, nil)) ;)
  (func $example_list_1
    (call $const (i32.const 125))
    (call $const (i32.const 127))
    (call $nil)
    (call $cons)
    (call $cons)
    (call $log_stack)
    (call $log_heap)
  )

  (func $singleton_plus_one_test_0
    (call $const (i32.const 67))
    (call $singleton_plus_one)
    (call $log_heap)
  )

  (func $is_zero_test_0
    (call $const (i32.const 0))
    (call $is_zero)
    (call $log_stack)
  )

  (func $is_zero_test_1
    (call $const (i32.const 56))
    (call $is_zero)
    (call $log_stack)
  )

  (func $env_test_0
    (call $const (i32.const 56))
    (call $const (i32.const 57))
    (call $make_env (i32.const 2))
    (call $log_stack)
  )

  (func $env_test_1
    (call $const (i32.const 56))
    (call $const (i32.const 57))
    (call $make_env (i32.const 2))
    (call $log_stack)

    (call $const (i32.const 60))
    (call $const (i32.const 61))
    (call $const (i32.const 62))
    (call $log_stack)

    (call $open_new_scope (i32.const 3))
    (call $log_stack)
  )

  (func $sum_test_0
    (call $const (i32.const 5))
    (call $sum)
    (call $log_stack)
  )


  (func $init
    (; (call $example_stack0) ;)
    (; (call $example_heap0) ;)
    (; (call $example_list_0) ;)
    (; (call $example_list_1) ;)
    (; (call $singleton_plus_one_test_0) ;)
    (; (call $is_zero_test_0) ;)
    (; (call $is_zero_test_1) ;)
    (; (call $env_test_0) ;)
    (call $env_test_1)
    (; (call $sum_test_0) ;)
  )
  (export "init" (func $init))
)
