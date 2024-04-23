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
  (global $STACK (mut i32) (i32.const 0))
  (export "stack" (global $STACK))
  (global $frame (mut i32) (i32.const 0))
  (export "frame" (global $frame))
  (global $ENV (mut i32) (i32.const 0))
  (export "env" (global $ENV))

  (global $heap (mut i32) (i32.const 1024)) ;; start of the heap
  (export "heap" (global $heap))
  (global $FREE (mut i32) (i32.const 1024)) ;; pointer to the next free cell on the heap
  (export "free" (global $FREE))

  (; ===Global Constants=== ;)
  (global $ENV_HEADER_BYTE_SIZE i32 (i32.const 4))
  (global $ENV_POINTER_BYTE_SIZE i32 (i32.const 4))
  (global $SCOPE_EXTENSION_HEADER_SIZE i32 (i32.const 8))

  (global $TAGGED_POINTER_BYTE_SIZE i32 (i32.const 5))
  (global $CONST_TAG i32 (i32.const 1))
  (global $ARRAY_TAG i32 (i32.const 2))
  (global $TUPLE_TAG i32 (i32.const 3))

  (global $GC_TAG_LIVE i32 (i32.const 0))
  (global $GC_TAG_MOVED i32 (i32.const 1))


  (func $inc_stack (param $count i32)
    (global.set $STACK (i32.add (global.get $STACK) (local.get $count)))
  )

  (func $dec_stack (param $count i32)
    (global.set $STACK (i32.sub (global.get $STACK) (local.get $count)))
  )

  (func $inc_free (param $count i32)
    (global.set $FREE (i32.add (global.get $FREE) (local.get $count)))
  )

  (; ===Constant=== ;)
  (; Pushes constant to Linear Stack ;)
  (func $const (param $variant i32)
    (i32.store8 (global.get $STACK) (global.get $CONST_TAG))
    (call $inc_stack (i32.const 1))

    (i32.store (global.get $STACK) (local.get $variant))
    (call $inc_stack (i32.const 4))
  )

  (; Assumes that a constant is on the top of the Linear Stack. ;)
  (; Moves it to WASM stack ;)
  (func $get_const (result i32)
    (call $dec_stack (i32.const 4))
    (i32.load (global.get $STACK))
    (call $dec_stack (i32.const 1))
  )

  (; ===Tuple=== ;)
  (; Consumes $count many tagged pointers on the stack, and moves them onto the heap ;)
  (; 1 <= $count <= 255 ;)
  (func $tuple (param $variant i32) (param $count i32)
    (local $component_byte_size i32)
    (local $tuple_pointer i32)

    (local.set $tuple_pointer (global.get $FREE))
    (; ==header== ;)
    (; 1 bit (takes the whole byte ofcourse) for garbage collection. ;)
    (i32.store8 (global.get $FREE) (global.get $GC_TAG_LIVE))
    (call $inc_free (i32.const 1))
    (; 4 bytes for variant ;)
    (i32.store (global.get $FREE) (local.get $variant))
    (call $inc_free (i32.const 4))
    (; 1 byte for number of components ;)
    (i32.store8 (global.get $FREE) (local.get $count))
    (call $inc_free (i32.const 1))
    
    (; ==payload== ;)
    (; go back to the start of the component on the stack ;)
    (local.set $component_byte_size (i32.mul (local.get $count) (global.get $TAGGED_POINTER_BYTE_SIZE)))
    (call $dec_stack (local.get $component_byte_size))

    (memory.copy (global.get $FREE) (global.get $STACK) (local.get $component_byte_size))
    (call $inc_free (local.get $component_byte_size))
    
    (; ==tagged pointer to the tuple on the stack== ;)
    (i32.store8 (global.get $STACK) (global.get $TUPLE_TAG))
    (call $inc_stack (i32.const 1))

    (i32.store (global.get $STACK) (local.get $tuple_pointer))
    (call $inc_stack (i32.const 4))
  )

  (; ===Environment=== ;)
  (; Consumes $count elements on the top of the stack ;)
  (; and creates an environment out of them ;)
  (func $make_env (param $count i32)
    (; This is terribly inefficient. We're just shifting the top of the stack ;)
    (; by $ENV_HEADER_BYTE_SIZE bytes, and then writing the size as the prefix. ;)

    (local $byte_size i32)
    (local $source i32)
    (local $destination i32)

    (local.set $byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $count)))
    (local.set $source (i32.sub (global.get $STACK) (local.get $byte_size)))
    (local.set $destination (i32.add (local.get $source) (global.get $ENV_HEADER_BYTE_SIZE)))

    (; ==Copy the tagged pointers== ;)
    (memory.copy (local.get $destination) (local.get $source) (local.get $byte_size))

    (; ==Set the header== ;)
    (i32.store (local.get $source) (local.get $count))

    (; ==Set the environment on the Linear Stack as the current environment== ;)
    (global.set $ENV (local.get $destination))
    
    (call $inc_stack (global.get $ENV_HEADER_BYTE_SIZE))
  )

  (; Assume N arguments on the stack (here N == $arg_count) ;)
  (; The N arguments are consumed, and in their place we have ;)
  (; * Saved current env register ;)
  (; * Followed by the new extended environment with the consumed arguments ;)
  (; The env register is updated to point to this new environment. ;)
  (func $open_new_scope (param $arg_count i32)
    (local $old_env_count i32)
    (local $old_env_byte_size i32)

    (; start of where the current arguments will be copied to ;)
    (local $destination_arg i32)
    (local $arg_byte_size i32)

    (local.set $old_env_count (i32.load (i32.sub (global.get $ENV) (global.get $ENV_HEADER_BYTE_SIZE))))
    (local.set $old_env_byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $old_env_count)))
    (local.set $arg_byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $arg_count)))

    (call $dec_stack (local.get $arg_byte_size))

    (; ==Copying the current arguments to the new env== ;)
    (local.set $destination_arg (i32.add (global.get $STACK) (i32.add (global.get $SCOPE_EXTENSION_HEADER_SIZE) (local.get $old_env_byte_size))))
    (memory.copy (local.get $destination_arg) (global.get $STACK) (local.get $arg_byte_size))

    (; ==Copying the old env== ;)
    (memory.copy (i32.add (global.get $STACK) (global.get $SCOPE_EXTENSION_HEADER_SIZE)) (global.get $ENV) (local.get $old_env_byte_size))

    (; ==Saving old ENV register== ;)
    (i32.store (global.get $STACK) (global.get $ENV))
    (call $inc_stack (global.get $ENV_POINTER_BYTE_SIZE))

    (; ==Updating the env register== ;) 
    (global.set $ENV (i32.add (global.get $STACK) (global.get $ENV_HEADER_BYTE_SIZE)))

    (; ==Prefixing the environment with the size ;)
    (i32.store (global.get $STACK) (i32.add (local.get $old_env_count) (local.get $arg_count)))

    (; ==Moving the stack== ;)
    (call $inc_stack (i32.add (global.get $ENV_HEADER_BYTE_SIZE) (i32.add (local.get $old_env_byte_size) (local.get $arg_byte_size))))
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
    (call $log_stack)
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
