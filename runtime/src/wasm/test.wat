(; memory.copy $destination $source $size ;)

;; TODO: Change `get` to `take` - this will indicate that the operation actually consumes the value on the Linear Stack
;;       Also introduce `read` functions that will just copy stuff from the linear stack to WASM stack without consumption.

(module
  (import "console" "log" (func $log (param i32)))
  (import "console" "logStack" (func $log_stack))
  (import "console" "logHeap" (func $log_heap))

  (memory 2 10)
  (export "memory" (memory 0))

  (table 50 funcref)


  (; Linear Stack := Linear Memory Stack ;)
  (global $stack_start (mut i32) (i32.const 0))
  (export "stack_start" (global $stack_start))
  (global $STACK (mut i32) (i32.const 0))
  (export "stack" (global $STACK))
  (global $frame (mut i32) (i32.const 0))
  (export "frame" (global $frame))
  (global $ENV (mut i32) (i32.const 0))
  (export "env" (global $ENV))

  (global $heap (mut i32) (i32.const 2048)) ;; start of the heap
  (export "heap" (global $heap))
  (global $FREE (mut i32) (i32.const 2048)) ;; pointer to the next free cell on the heap
  (export "free" (global $FREE))

  (; ===Global Constants=== ;)
  (global $ENV_HEADER_BYTE_SIZE i32 (i32.const 4))
  (global $ENV_POINTER_BYTE_SIZE i32 (i32.const 4))
  (global $SCOPE_EXTENSION_HEADER_SIZE i32 (i32.const 8))

  (global $TAGGED_POINTER_BYTE_SIZE i32 (i32.const 5))
  (global $CONST_TAG i32 (i32.const 1))
  (global $ARRAY_TAG i32 (i32.const 2))
  (global $TUPLE_TAG i32 (i32.const 3))

  (global $TUPLE_HEADER_OFFSET i32 (i32.const 6)) ;; 6 bytes, 1 byte for GC bit, 4 bytes for variant, 1 byte for count
  (global $TUPLE_VARIANT_OFFSET i32 (i32.const 1))
  (global $TUPLE_COUNT_OFFSET i32 (i32.const 5))


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
  ;; TODO: Maybe I should have another function with a check
  ;;       that the tagged pointer is really a constant
  ;;       that would trigger a trap otherwise?
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

  (; Assumes that a pointer to a tuple is on the top of the Linear Stack. ;)
  (; Moves its to WASM stack ;)
  (func $get_tuple_pointer (result i32)
    (call $dec_stack (i32.const 4))
    (i32.load (global.get $STACK))
    (call $dec_stack (i32.const 1))
  )

  (func $get_tuple_variant (result i32)
    (i32.load (i32.add (call $get_tuple_pointer) (global.get $TUPLE_VARIANT_OFFSET)))
  )

  (; On top of the stack there is a tagged pointer to a tuple on the heap. ;)
  (; Moves its $index-th component onto the Linear Stack ;)
  (func $tuple_project (param $index i32)
    (local $offset i32)
    (local $source i32)

    (local.set $offset (i32.add (global.get $TUPLE_HEADER_OFFSET) (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $index))))
    ;; WARNING: $get_tuple_pointer decrements the $STACK register!
    (local.set $source (i32.add (call $get_tuple_pointer) (local.get $offset)))

    (memory.copy
      (global.get $STACK) ;; Note that the tagged pointer to the tuple was already consumed
      (local.get $source)
      (global.get $TAGGED_POINTER_BYTE_SIZE))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )

  (; ==Tagged Pointers== ;)
  (; Given a tagged pointer, returns the tag on WASM stack ;)
  (func $read_tag (result i32)
    (call $dec_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
    (i32.load8_u (global.get $STACK))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )

  (; Given a tagged pointer, ;)
  (; case it is const, moves the variant on WASM stack. ;)
  (; case it is tagged pointer, dereferences the pointer and copies the variant on WASM stack ;)
  (func $get_variant (result i32)
    (i32.eq (call $read_tag) (global.get $CONST_TAG))
    (if (result i32)
      (then
        (call $get_const)
      )
      (else
        (i32.eq (call $read_tag) (global.get $TUPLE_TAG))
        (if (result i32)
          (then
            (call $get_tuple_variant)
          )
          (else
            unreachable
          )
        )
      )
    )
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
    (call $dec_stack (local.get $byte_size))
    (local.set $destination (i32.add (global.get $STACK) (global.get $SCOPE_EXTENSION_HEADER_SIZE)))

    (; ==Copy the arguments== ;)
    (memory.copy (local.get $destination) (global.get $STACK) (local.get $byte_size))

    (; ==Saving old ENV register== ;)
    (i32.store (global.get $STACK) (global.get $ENV))
    (call $inc_stack (global.get $ENV_POINTER_BYTE_SIZE))

    (; ==Set the size== ;)
    (i32.store (global.get $STACK) (local.get $count))
    (call $inc_stack (global.get $ENV_HEADER_BYTE_SIZE))

    (; ==Set the environment on the Linear Stack as the current environment== ;)
    (global.set $ENV (global.get $STACK))
    (call $inc_stack (local.get $byte_size))
  )

  (; Assume N arguments on the stack (here N == $arg_count) ;)
  (; The N arguments are consumed, and in their place we have ;)
  (; * Environment header (saved current ENV register followed by the count of variables) ;)
  (; * Followed by the new extended environment with the consumed arguments ;)
  (; The env register is updated to point to this new environment. ;)
  (func $extend_env (param $arg_count i32)
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

  (; copies the n-th var in the current env to the top of the current stack ;)
  (func $var (param $index i32)
    (memory.copy
      (global.get $STACK)
      (i32.add (global.get $ENV) (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $index)))
      (global.get $TAGGED_POINTER_BYTE_SIZE))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )

  (func $drop_env
    (local $header_start i32)
    (local.set $header_start (i32.sub (global.get $ENV) (global.get $SCOPE_EXTENSION_HEADER_SIZE)))

    (; ==Reset env== ;)
    (global.set $ENV (i32.load (local.get $header_start)))

    (memory.copy
      (local.get $header_start)
      (i32.sub (global.get $STACK) (global.get $TAGGED_POINTER_BYTE_SIZE))
      (global.get $TAGGED_POINTER_BYTE_SIZE))

    (global.set $STACK (i32.add (local.get $header_start) (global.get $TAGGED_POINTER_BYTE_SIZE)))
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
    (call $var (i32.const 0)) ;; n
    (call $get_const)
    (if (i32.eqz)
      (then
        (call $const (i32.const 0))
      )
      (else
        (call $var (i32.const 0)) ;; n
        (call $dec)
        (call $make_env (i32.const 1)) ;; n - 1
        (call $sum)
        (call $drop_env) ;; sum(n - 1)

        (call $var (i32.const 0)) ;; n
        (call $add) ;; sum(n - 1) + n
      )
    )
  )

  (; fn rangeLoop(n, xs) { ;)
  (;  match n { ;)
  (;  | Const(0) -> xs ;)
  (;  | _ -> ;)
  (;      let m = call dec(n) ;)
  (;      call rangeLoop(m, cons(m, xs))) ;)
  (;  } ;)
  (; } ;)
  (func $range_loop
    (call $var (i32.const 0)) ;; n
    (call $get_const)
    (if (i32.eqz)
      (then
        (call $var (i32.const 1)) ;; xs
      )
      (else
        (call $var (i32.const 0)) ;; n
        (call $dec) ;; n - 1
        ;; Start Let
        (call $extend_env (i32.const 1)) ;; let m := n - 1
        ;; now our environment is n, xs, m

        (call $var (i32.const 2)) ;; m

        (call $var (i32.const 2)) ;; m
        (call $var (i32.const 1)) ;; xs
        (call $cons)

        (call $make_env (i32.const 2)) ;; range_loop(m, cons(m, xs))
        (call $range_loop)
        (call $drop_env)

        ;; End Let
        (call $drop_env)
      )
    )
  )

  (func $head_unsafe
    (call $tuple_project (i32.const 0))
  )

  (func $tail_unsafe
    (call $tuple_project (i32.const 1))
  )

  (; fn range(n) { ;)
  (;   call rangeLoop(n, Const(Nil)) ;)
  (; } ;)
  (func $range
    (call $var (i32.const 0)) ;; n
    (call $const (i32.const 0)) ;; xs := nil
    (call $make_env (i32.const 2))
    (call $range_loop)
    (call $drop_env)
  )

  (; fn log_list(xs) { ;)
  (;  match xs { ;)
  (;  | Const(nil) -> log(5555555) ;)
  (;  | _ -> ;)
  (;    call log(call head(xs)) ;)
  (;    call log_list(call tail(xs)) ;)
  (;  } ;)
  (; } ;)
  (func $log_list ;; list of integers
    (call $var (i32.const 0)) ;; xs
    (call $get_variant)
    (if (i32.eqz)
      (then ;; nil branch
        (call $log (i32.const 5555555)) ;; right now this is how we print empty list
      )
      (else ;; cons branch
        (call $var (i32.const 0)) ;; xs
        (call $tuple_project (i32.const 0)) ;; head(xs)
        (call $log (call $get_const))

        (call $var (i32.const 0)) ;; xs
        (call $tuple_project (i32.const 1)) ;; head(xs)
        (call $make_env (i32.const 1))
        (call $log_list) ;; log_list(tail(xs))
        (call $drop_env)
      )
    )
  )

  (; ==tests== ;)
  (func $example_stack_0
    (call $const (i32.const 5))
    (call $const (i32.const 7))
    (call $log_stack)
    (call $add)
    (call $log_stack)
    (; (call $log (call $get_const)) ;)
    (; (call $log (call $get_const)) ;)
  )

  (func $example_heap_0
    (call $const (i32.const 15))
    (call $const (i32.const 16))
    (call $log_stack)

    (call $tuple (i32.const 4) (i32.const 2))
    (call $log_stack)
    (call $log_heap)
  )

  (func $tuple_test_0
    (call $const (i32.const 15))
    (call $const (i32.const 16))
    (call $log_stack)

    (call $tuple (i32.const 25) (i32.const 2))
    (call $log_stack)
    (call $log_heap)

    (; (call $get_tuple_pointer) ;)
    (; (call $get_tuple_variant) ;)
    (; (call $log) ;)
    (call $tuple_project (i32.const 1))
    (call $log_stack)
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

    (call $extend_env (i32.const 3))
    (call $log_stack)
  )

  (func $env_test_2
    (call $const (i32.const 25))
    (call $const (i32.const 26))
    (call $const (i32.const 27))
    (call $make_env (i32.const 3))
    (call $log_stack)

    (call $var (i32.const 2))
    (call $log_stack)
  )

  (func $env_test_3
    (call $const (i32.const 25))
    (call $const (i32.const 27))
    (call $log_stack)
    (call $make_env (i32.const 2))

    (call $log_stack)

    (call $var (i32.const 0))
    (call $log_stack)
    (call $var (i32.const 1))
    (call $log_stack)
    (call $add)
    (call $log_stack)

    (call $drop_env)
    (call $log_stack)
  )

  (func $sum_test_0
    (call $const (i32.const 8))
    (call $make_env (i32.const 1))
    (call $sum)
    (call $drop_env)

    (call $log_stack)
  )

  (func $range_test_0
    (call $const (i32.const 5)) ;; n  := 5
    (call $make_env (i32.const 1))
    (call $range)
    (call $drop_env)

    (call $make_env (i32.const 1))
    (call $log_list)
    (call $drop_env)
  )


  (func $tables_test_0
    (call_indirect (i32.const 2))
  )

  (; (elem (i32.const 0) $add $inc $range_test_0) ;)
  (elem (i32.const 0) $add)
  (elem (i32.const 1) $inc)
  (elem (i32.const 2) $range_test_0)

  (export "example_stack_0" (func $example_stack_0))
  (export "example_heap_0" (func $example_heap_0))
  (export "tuple_test_0" (func $tuple_test_0))
  (export "example_list_0" (func $example_list_0))
  (export "example_list_1" (func $example_list_1))
  (export "singleton_plus_one_test_0" (func $singleton_plus_one_test_0))

  (export "is_zero_test_0" (func $is_zero_test_0))
  (export "is_zero_test_1" (func $is_zero_test_1))

  (export "env_test_0" (func $env_test_0))
  (export "env_test_1" (func $env_test_1))
  (export "env_test_2" (func $env_test_2))
  (export "env_test_3" (func $env_test_3))

  (export "sum_test_0" (func $sum_test_0))
  (export "range_test_0" (func $range_test_0))
)
