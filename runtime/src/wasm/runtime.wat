;; TODO: Change `get` to `take` - this will indicate that the operation actually consumes the value on the Linear Stack
;;       Also introduce `read` functions that will just copy stuff from the linear stack to WASM stack without consumption.

(module
  (import "env" "memory" (memory $main_memory 0))
  (import "env" "STATIC_SIZE" (global $STATIC_SIZE i32)) ;; TODO: recompute everything with respect to STATIC...? in js... You also need to export it from the compiler.
  (import "env" "STACK_SIZE" (global $STACK_SIZE i32))
  (import "env" "HEAP_SIZE" (global $HEAP_SIZE i32))
  (import "env" "STACK_START" (global $STACK_START i32))
  (import "env" "A_HEAP_START" (global $A_HEAP_START (mut i32)))
  (import "env" "B_HEAP_START" (global $B_HEAP_START (mut i32)))
  (import "env" "FREE" (global $FREE (mut i32))) ;; pointer to the next free cell on the heap

  (import "env" "on_stack_overflow" (func $on_stack_overflow))
  (import "env" "on_garbage_collection" (func $on_garbage_collection))
  (import "env" "on_out_of_memory" (func $on_out_of_memory))

  (import "env" "debug" (func $debug))
  (import "env" "debug_int" (func $debug_int (param i32)))
  (import "env" "show_stack" (func $show_stack (param i32)))

  (import "primitives" "perform_primitive_command" (func $perform_primitive_command (param i32)))

  (global $STACK (mut i32) (global.get $STACK_START))
  (export "stack" (global $STACK))
  (global $ENV (mut i32) (global.get $STACK_START))
  (export "env" (global $ENV))

  (; ===Global Constants=== ;)
  (global $TAGGED_POINTER_BYTE_SIZE i32 (i32.const 5))

  (global $ENV_TAG i32 (i32.const 0))
  (global $CONST_TAG i32 (i32.const 1))
  (global $TUPLE_TAG i32 (i32.const 2))
  (global $BYTE_ARRAY_TAG i32 (i32.const 3))
  (global $BYTE_ARRAY_SLICE_TAG i32 (i32.const 4))

  (global $GC_TAG_BYTE_SIZE i32 (i32.const 1))
  (global $TAG_BYTE_SIZE i32 (i32.const 1))

  ;; | tag 1 byte | env pointer 4 bytes | count 4 bytes |  ... | sequence of tagged pointers/constants, 5 bytes each | ...
  (global $ENV_HEADER_BYTE_SIZE i32 (i32.const 9)) ;; 9 bytes, 1 byte for tag, 4 bytes for parent env pointer, 4 bytes for env count.
  (global $PARENT_ENV_POINTER_BYTE_SIZE i32 (i32.const 4))
  (global $ENV_COUNTER_BYTE_SIZE i32 (i32.const 4))
  (; Env offset is relative to the current env pointer, which points to the start of the env-arguments. ;)
  (global $PARENT_ENV_POINTER_OFFSET i32 (i32.const -8))
  (global $ENV_COUNTER_OFFSET i32 (i32.const -4))
  (global $ENV_HEADER_START_OFFSET i32 (i32.const -9))

  ;; | gc 1 byte | tag 1 byte | variant 4 bytes | count 1 byte | ... | sequence of tagged pointers/constants, 5 bytes each | ...
  (global $TUPLE_HEADER_BYTE_SIZE i32 (i32.const 7)) ;; 6 bytes, 1 byte for GC bit, 4 bytes for variant, 1 byte for count
  (global $TUPLE_VARIANT_OFFSET i32 (i32.const 2))
  (global $TUPLE_COUNT_OFFSET i32 (i32.const 6))
  ;; TODO: Change this to $TUPLE_COMPONENTS_OFFSET
  (global $TUPLE_HEADER_OFFSET i32 (i32.const 7))

  (global $TUPLE_VARIANT_BYTE_SIZE i32 (i32.const 4))
  (global $TUPLE_COUNT_BYTE_SIZE i32 (i32.const 1))

  ;; | gc 1 byte | tag 1 byte | count 4 byte | string contents |
  (global $BYTE_ARRAY_HEADER_BYTE_SIZE i32 (i32.const 6))
  (global $BYTE_ARRAY_COUNT_OFFSET i32 (i32.const 2))
  (global $BYTE_ARRAY_CONTENTS_OFFSET i32 (i32.const 6))
  (global $BYTE_ARRAY_COUNT_SIZE i32 (i32.const 4))
  ;; | gc 1 byte | tag 1 byte | 1st pointer 4 byte | 2nd pointer 4 byte | count 4 byte |
  (global $SLICE_BYTE_SIZE i32 (i32.const 14))
  (global $SLICE_PARENT_POINTER_OFFSET i32 (i32.const 2))
  (global $SLICE_POINTER_OFFSET i32 (i32.const 6))
  (global $SLICE_COUNT_OFFSET i32 (i32.const 10))
  (global $SLICE_PARENT_POINTER_SIZE i32 (i32.const 4))
  (global $SLICE_POINTER_SIZE i32 (i32.const 4))
  (global $SLICE_COUNT_SIZE i32 (i32.const 4))

  (global $GC_TAG_LIVE i32 (i32.const 0))
  (global $GC_TAG_MOVED i32 (i32.const 1))


  (; ===Stack/Heap=== ;)
  (func $inc_stack (param $count i32)
    (if (i32.lt_u (i32.add (global.get $STACK) (local.get $count)) (global.get $STACK_SIZE))
      (then
        (global.set $STACK (i32.add (global.get $STACK) (local.get $count))))
      (else
        (call $on_stack_overflow)))
  )

  (func $dec_stack (param $count i32)
    (global.set $STACK (i32.sub (global.get $STACK) (local.get $count)))
  )

  (func $inc_free (param $count i32)
    (global.set $FREE (i32.add (global.get $FREE) (local.get $count)))
  )

  ;; Assume x is either a constant or tagged pointer.
  ;;   stack = [..., x]
  ;; ~>
  ;;   stack = [..., x, x]
  (func $dup
    (memory.copy (global.get $STACK)
                 (i32.sub (global.get $STACK) (global.get $TAGGED_POINTER_BYTE_SIZE))
                 (global.get $TAGGED_POINTER_BYTE_SIZE))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )
  (export "dup" (func $dup))

  (; ===Constant=== ;)
  (; Pushes constant to Linear Stack ;)
  (func $const (param $variant i32)
    (i32.store8 (global.get $STACK) (global.get $CONST_TAG))
    (call $inc_stack (i32.const 1))

    (i32.store (global.get $STACK) (local.get $variant))
    (call $inc_stack (i32.const 4))
  )
  (export "const" (func $const))

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
  (export "get_const" (func $get_const))

  (func $float32 (param $variant f32)
    (i32.store8 (global.get $STACK) (global.get $CONST_TAG))
    (call $inc_stack (i32.const 1))

    (f32.store (global.get $STACK) (local.get $variant))
    (call $inc_stack (i32.const 4))
  )
  (export "float32" (func $float32))

  (func $get_float32 (result f32)
    (call $dec_stack (i32.const 4))
    (f32.load (global.get $STACK))
    (call $dec_stack (i32.const 1))
  )
  (export "get_float32" (func $get_float32))

  (; ===Tuple=== ;)
  (; Consumes $count many tagged pointers on the stack, and moves them onto the heap ;)
  (; 0 <= $count <= 255 ;)
  (func $tuple (param $variant i32) (param $count i32)
    (local $component_byte_size i32)
    (local $tuple_pointer i32)

    (; ==check if we have enough space on the heap== ;)
    (local.set $component_byte_size (i32.mul (local.get $count) (global.get $TAGGED_POINTER_BYTE_SIZE)))
    (call $gc_if_out_of_space (i32.add (global.get $TUPLE_HEADER_BYTE_SIZE) (local.get $component_byte_size)))

    (local.set $tuple_pointer (global.get $FREE))
    (; ==header== ;)
    (; 1 bit (takes the whole byte ofcourse) for garbage collection liveness. ;)
    (i32.store8 (global.get $FREE) (global.get $GC_TAG_LIVE))
    (call $inc_free (global.get $GC_TAG_BYTE_SIZE))
    ;; tag (used by gc going from grey ~> black)
    (i32.store8 (global.get $FREE) (global.get $TUPLE_TAG))
    (call $inc_free (global.get $TAG_BYTE_SIZE))
    (; 4 bytes for variant ;)
    (i32.store (global.get $FREE) (local.get $variant))
    (call $inc_free (global.get $TUPLE_VARIANT_BYTE_SIZE))
    (; 1 byte for number of components ;)
    (i32.store8 (global.get $FREE) (local.get $count))
    (call $inc_free (global.get $TUPLE_COUNT_BYTE_SIZE))
    
    (; ==payload== ;)
    (; go back to the start of the component on the stack ;)
    (call $dec_stack (local.get $component_byte_size))

    (memory.copy (global.get $FREE) (global.get $STACK) (local.get $component_byte_size))
    (call $inc_free (local.get $component_byte_size))
    
    (; ==Push tagged pointer to the tuple on the stack== ;)
    (i32.store8 (global.get $STACK) (global.get $TUPLE_TAG))
    (call $inc_stack (global.get $TAG_BYTE_SIZE))

    (i32.store (global.get $STACK) (local.get $tuple_pointer))
    (call $inc_stack (i32.const 4))
  )
  (export "tuple" (func $tuple))

  (; Assumes that a pointer to a tuple is on the top of the Linear Stack. ;)
  (; Moves it to WASM stack ;)
  (func $get_tuple_pointer (result i32)
    (call $dec_stack (i32.const 4))
    (i32.load (global.get $STACK))
    (call $dec_stack (i32.const 1))
  )
  (export "get_tuple_pointer" (func $get_tuple_pointer))

  (func $get_tuple_variant (result i32)
    (i32.load (i32.add (call $get_tuple_pointer) (global.get $TUPLE_VARIANT_OFFSET)))
  )
  (export "get_tuple_variant" (func $get_tuple_variant))

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
  (export "tuple_project" (func $tuple_project))

  (; ==Tagged Pointers== ;)
  (; Given a tagged pointer, returns the tag on WASM stack ;)
  (func $read_tag (result i32)
    (call $dec_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
    (i32.load8_u (global.get $STACK))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )
  (export "read_tag" (func $read_tag))

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
  (export "get_variant" (func $get_variant))

  (; ===Environment=== ;)
  (; Consumes $count elements on the top of the stack ;)
  (; and creates an environment out of them ;)
  (func $make_env (param $count i32)
    (; This is terribly inefficient. We're just shifting the top of the stack ;)
    (; by $ENV_COUNTER_BYTE_SIZE bytes, and then writing the size as the prefix. ;)

    (local $byte_size i32)
    (local $source i32)
    (local $destination i32)

    (local.set $byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $count)))
    (call $dec_stack (local.get $byte_size))
    (local.set $destination (i32.add (global.get $STACK) (global.get $ENV_HEADER_BYTE_SIZE)))

    (; ==Copy the arguments== ;)
    (memory.copy (local.get $destination) (global.get $STACK) (local.get $byte_size))

    (; ==Tagging== ;)
    (i32.store8 (global.get $STACK) (global.get $ENV_TAG))
    (call $inc_stack (i32.const 1))

    (; ==Saving old ENV register== ;)
    (i32.store (global.get $STACK) (global.get $ENV))
    (call $inc_stack (global.get $PARENT_ENV_POINTER_BYTE_SIZE))

    (; ==Set the size== ;)
    (i32.store (global.get $STACK) (local.get $count))
    (call $inc_stack (global.get $ENV_COUNTER_BYTE_SIZE))

    (; ==Set the environment on the Linear Stack as the current environment== ;)
    (global.set $ENV (global.get $STACK))
    (call $inc_stack (local.get $byte_size))
  )
  (export "make_env" (func $make_env))

  (func $make_env_from (param $pointer i32) (param $count i32)
    (local $byte_size i32)
    (local $source i32)
    (local $destination i32)

    (local.set $byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $count)))
    (local.set $destination (i32.add (global.get $STACK) (global.get $ENV_HEADER_BYTE_SIZE)))

    (; ==Copy the arguments== ;)
    (memory.copy
      (i32.add (global.get $STACK) (global.get $ENV_HEADER_BYTE_SIZE))
      (local.get $pointer)
      (local.get $byte_size))

    (; ==Tagging== ;)
    (i32.store8 (global.get $STACK) (global.get $ENV_TAG))
    (call $inc_stack (i32.const 1))

    (; ==Saving old ENV register== ;)
    (i32.store (global.get $STACK) (global.get $ENV))
    (call $inc_stack (global.get $PARENT_ENV_POINTER_BYTE_SIZE))

    (; ==Set the size== ;)
    (i32.store (global.get $STACK) (local.get $count))
    (call $inc_stack (global.get $ENV_COUNTER_BYTE_SIZE))

    (; ==Set the environment on the Linear Stack as the current environment== ;)
    (global.set $ENV (global.get $STACK))

    (call $inc_stack (local.get $byte_size))
  )
  (export "make_env_from" (func $make_env_from))

  (; Assume N arguments on the stack (here N == $arg_count) ;)
  (; The N arguments are consumed, and in their place we have ;)
  (; * Environment header ;)
  (; * Followed by the new extended environment with the consumed arguments ;)
  (; The env register is updated to point to this new environment. ;)
  (func $copy_and_extend_env (param $arg_count i32)
    (local $old_env_count i32)
    (local $old_env_arg_byte_size i32)

    (; start of where the current arguments will be copied to ;)
    (local $destination_arg i32)
    (local $arg_byte_size i32)

    (local.set $old_env_count (i32.load (i32.add (global.get $ENV) (global.get $ENV_COUNTER_OFFSET))))
    (local.set $old_env_arg_byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $old_env_count)))
    (local.set $arg_byte_size (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $arg_count)))

    ;; stack == [..., a0, a1, a2]
    (call $dec_stack (local.get $arg_byte_size))
    ;; stack == [..., ], a0, a1, a2

    (; ==Copying the current arguments to the new env== ;)
    (local.set $destination_arg (i32.add (global.get $STACK) (i32.add (global.get $ENV_HEADER_BYTE_SIZE) (local.get $old_env_arg_byte_size))))
    ;; stack == [..., ], a0, a1, a2
    (memory.copy (local.get $destination_arg) (global.get $STACK) (local.get $arg_byte_size))
    ;; stack == [..., reserved space for old env], a0, a1, a2

    (; ====Copying the old env==== ;)
    (; ==Tagging== ;)
    (i32.store8 (global.get $STACK) (global.get $ENV_TAG))
    (call $inc_stack (i32.const 1))

    (; ==Saving old ENV register== ;)
    (i32.store (global.get $STACK) (global.get $ENV))
    (call $inc_stack (global.get $PARENT_ENV_POINTER_BYTE_SIZE))

    (; ==Set the size== ;)
    (i32.store (global.get $STACK) (i32.add (local.get $old_env_count) (local.get $arg_count)))
    (call $inc_stack (global.get $ENV_COUNTER_BYTE_SIZE))

    (; ==Copy old args== ;)
    (memory.copy (global.get $STACK) (global.get $ENV) (local.get $old_env_arg_byte_size))

    (; ==Set the environment on the Linear Stack as the current environment== ;)
    (global.set $ENV (global.get $STACK))

    (call $inc_stack (i32.add (local.get $old_env_arg_byte_size) (local.get $arg_byte_size)))
  )
  (export "copy_and_extend_env" (func $copy_and_extend_env))

  (; Increments the current env counter by $count. ;)
  (; This can be used in the situation where after the top of linear stack there is some value that needs to be ;)
  (; included in the current environment, and the environment is the top value on the linear stack. ;)
  (func $extend_env (param $count i32)
    (local $env_count i32)
    (local.set $env_count (i32.load (i32.add (global.get $ENV) (global.get $ENV_COUNTER_OFFSET))))
    (i32.store (i32.add (global.get $ENV) (global.get $ENV_COUNTER_OFFSET))
               (i32.add (local.get $env_count) (local.get $count)))
  )
  (export "extend_env" (func $extend_env))

  (; copies the n-th var in the current env to the top of the current stack ;)
  (func $var (param $index i32)
    (memory.copy
      (global.get $STACK)
      (i32.add (global.get $ENV) (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $index)))
      (global.get $TAGGED_POINTER_BYTE_SIZE))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )
  (export "var" (func $var))

  ;;   stack = [..., current_env, some_stuff0, some_stuff1, ret_value]
  ;; ~>
  ;;   stack = [..., ret_value]
  ;; Note how some_stuff0, some_stuff1 is nuked from the stack.
  ;; TODO: Rename to $drop_env_then_shift_top
  (func $drop_env
    (local $header_start i32)
    (local.set $header_start (i32.add (global.get $ENV) (global.get $ENV_HEADER_START_OFFSET)))

    (; ==Reset env== ;)
    (global.set $ENV (i32.load (i32.add (global.get $ENV) (global.get $PARENT_ENV_POINTER_OFFSET))))

    (memory.copy
      (local.get $header_start)
      (i32.sub (global.get $STACK) (global.get $TAGGED_POINTER_BYTE_SIZE))
      (global.get $TAGGED_POINTER_BYTE_SIZE))

    (global.set $STACK (i32.add (local.get $header_start) (global.get $TAGGED_POINTER_BYTE_SIZE)))
  )
  (export "drop_env" (func $drop_env))

  ;;   stack = [..., current_env, some_stuff0, some_stuff1, some_stuff1]
  ;; ~>
  ;;   stack = [..., some_stuff0, some_stuff1, some_stuff1]
  ;; Useful for tail-calls.
  (func $drop_env_then_shift
    (local $header_start i32)
    (local $after_env_start i32)
    (local $stuff_after_env_in_bytes i32)

    (local.set $header_start (i32.add (global.get $ENV) (global.get $ENV_HEADER_START_OFFSET)))
    (local.set $after_env_start (i32.add (global.get $ENV) (i32.mul (i32.load (i32.add (global.get $ENV) (global.get $ENV_COUNTER_OFFSET))) (global.get $TAGGED_POINTER_BYTE_SIZE))))
    (local.set $stuff_after_env_in_bytes (i32.sub (global.get $STACK) (local.get $after_env_start)))

    (; ==Reset env== ;)
    (global.set $ENV (i32.load (i32.add (global.get $ENV) (global.get $PARENT_ENV_POINTER_OFFSET))))

    (memory.copy
      (local.get $header_start)
      (local.get $after_env_start)
      (local.get $stuff_after_env_in_bytes))

    (global.set $STACK (i32.add (local.get $header_start) (local.get $stuff_after_env_in_bytes)))
  )
  (export "drop_env_then_shift" (func $drop_env_then_shift))

  (; Bundle a function pointer together with $arg_count many values (which are assummed to be on the linear stack) and put in on the heap. ;)
  (func $partial_apply (param $fn_index i32) (param $arg_count i32)
    (; Stores closure as a tuple whose components are the arguments, and whose function pointer is the tuple's variant. ;)
    (call $tuple (local.get $fn_index) (local.get $arg_count))
  )
  (export "partial_apply" (func $partial_apply))

  (; Assume on top of the linear stack there is $arg_count many arguments and a closure below them. ;)
  (; Copy closure's partial environment and extend it with the arguments. Then return the function_pointer. ;)
  (func $make_env_from_closure (param $arg_count i32) (result i32)
    (local $closure_pointer i32)
    (local $closure_env_count i32)
    (local $fn_pointer i32)

    ;; stack == [..., closure_pointer, b0, b1, b2]
    (call $dec_stack (i32.mul (local.get $arg_count) (global.get $TAGGED_POINTER_BYTE_SIZE)))
    ;; stack == [..., closure_pointer], b0, b1, b2
    (local.set $closure_pointer (call $get_tuple_pointer))
    ;; stack == [...], closure_pointer, b0, b1, b2
    (local.set $closure_env_count (i32.load8_u (i32.add (local.get $closure_pointer) (global.get $TUPLE_COUNT_OFFSET))))
    (local.set $fn_pointer (i32.load (i32.add (local.get $closure_pointer) (global.get $TUPLE_VARIANT_OFFSET))))

    ;; Shift arguments on the stack so there's enough room for closure's environment.
    ;; stack == [...], closure_pointer, b0, b1, b2
    (memory.copy
      (i32.add (global.get $STACK) (i32.add (global.get $ENV_HEADER_BYTE_SIZE) (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $closure_env_count))))
      (i32.add (global.get $STACK) (global.get $TAGGED_POINTER_BYTE_SIZE))
      (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $arg_count)))
    ;; stack == [...], reserved space for closure env, b0, b1, b2

    ;; Open closure's environment.
    (call $make_env_from (i32.add (local.get $closure_pointer) (global.get $TUPLE_HEADER_OFFSET)) (local.get $closure_env_count))
    ;; stack == [..., closure_env[a0, a1]], b0, b1, b2

    (; ===Increment the env on top of the stack by argument count=== ;)
    (i32.store
        (i32.add (global.get $ENV) (global.get $ENV_COUNTER_OFFSET))
        (i32.add
          (i32.load (i32.add (global.get $ENV) (global.get $ENV_COUNTER_OFFSET)))
          (local.get $arg_count)))

    (call $inc_stack (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (local.get $arg_count)))

    (local.get $fn_pointer)
  )
  (export "make_env_from_closure" (func $make_env_from_closure))

  ;; Pushes the contents at $raw_pointer onto the linear stack in the reverse order.
  (func $unpack_in_reverse (param $raw_pointer i32) (param $count i32)
    (if (i32.eq (local.get $count) (i32.const 0))
      (then nop)
      (else
        (memory.copy
          (global.get $STACK)
          (i32.add (local.get $raw_pointer) (i32.mul (global.get $TAGGED_POINTER_BYTE_SIZE) (i32.sub (local.get $count) (i32.const 1))))
          (global.get $TAGGED_POINTER_BYTE_SIZE))
        (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
        (call $unpack_in_reverse (local.get $raw_pointer) (i32.sub (local.get $count) (i32.const 1)))))
  )
  (export "unpack_in_reverse" (func $unpack_in_reverse))

  (func $copy_value_to_stack (param $raw_pointer i32)
    (memory.copy
      (global.get $STACK)
      (local.get $raw_pointer)
      (global.get $TAGGED_POINTER_BYTE_SIZE))
    (call $inc_stack (global.get $TAGGED_POINTER_BYTE_SIZE))
  )
  (export "copy_value_to_stack" (func $copy_value_to_stack))

  (func $pure
    ;; a -> Cmd(a)
    ;; op-code for pure is 0
    (call $tuple (i32.const 0) (i32.const 1))
  )
  (export "pure" (func $pure))

  ;; Assumes linear stack [..., closure_pointer, command]
  (func $and_then
    ;; Cmd(a), (a -> Cmd(b)) -> Cmd(b)
    ;; op-code for and_then is 1
    (call $tuple (i32.const 1) (i32.const 2))
  )
  (export "and_then" (func $and_then))

  (; =====Byte Arrays and Slices====== ;)
  ;; Create an array slice on the heap, then push a tagged pointer to it to the stack.
  (func $array_slice (param $slice_parent_pointer i32) (param $slice_pointer i32) (param $slice_count i32)
    (local $pointer_to_slice i32)
    ;; TODO
    (call $gc_if_out_of_space (global.get $SLICE_BYTE_SIZE))

    (local.set $pointer_to_slice (global.get $FREE))

    (i32.store8 (global.get $FREE) (global.get $GC_TAG_LIVE))

    ;; 1 byte for garbage collection liveness.
    (i32.store8 (global.get $FREE) (global.get $GC_TAG_LIVE))
    (call $inc_free (global.get $GC_TAG_BYTE_SIZE))
    ;; tag (used by gc going from grey ~> black)
    (i32.store8 (global.get $FREE) (global.get $BYTE_ARRAY_SLICE_TAG))
    (call $inc_free (global.get $TAG_BYTE_SIZE))

    (; 4 bytes for slice_parent_pointer;)
    (i32.store (global.get $FREE) (local.get $slice_parent_pointer))
    (call $inc_free (global.get $SLICE_PARENT_POINTER_SIZE))

    (; 4 bytes for slice_pointer ;)
    (i32.store (global.get $FREE) (local.get $slice_pointer))
    (call $inc_free (global.get $SLICE_POINTER_SIZE))

    (; 4 bytes for slice_count ;)
    (i32.store (global.get $FREE) (local.get $slice_count))
    (call $inc_free (global.get $SLICE_COUNT_SIZE))

    ;; ==Push tagged pointer to the slice on the stack==
    (i32.store8 (global.get $STACK) (global.get $BYTE_ARRAY_SLICE_TAG))
    (call $inc_stack (global.get $TAG_BYTE_SIZE))

    (i32.store (global.get $STACK) (local.get $pointer_to_slice))
    (call $inc_stack (i32.const 4))
  )
  (export "array_slice" (func $array_slice))

  ;;   stack = [..., pointer to slice]
  ;; ~>
  ;;   stack = [...]
  (func $get_array_slice_pointer (result i32)
    (call $dec_stack (i32.const 4))
    (i32.load (i32.add (i32.load (global.get $STACK)) (global.get $SLICE_POINTER_OFFSET)))
    (call $dec_stack (i32.const 1))
  )
  (export "get_array_slice_pointer" (func $get_array_slice_pointer))

  ;;   stack = [..., pointer to slice]
  ;; ~>
  ;;   stack = [...]
  (func $get_array_slice_count (result i32)
    (call $dec_stack (i32.const 4))
    (i32.load (i32.add (i32.load (global.get $STACK)) (global.get $SLICE_COUNT_OFFSET)))
    (call $dec_stack (i32.const 1))
  )
  (export "get_array_slice_count" (func $get_array_slice_count))


  ;; Copies and concatenates two arrays into a new location on the heap. Returns raw_pointer to the new array.
  ;; The raw pointers point to directly to the start of the byte contents (not to the start of an array), since we may get these data through array slices.
  (func $concat_arrays (param $raw_pointer_0 i32) (param $count_0 i32) (param $raw_pointer_1 i32) (param $count_1 i32) (result i32)
    (local $result_array_count i32)
    (local $result_pointer i32)

    (local.set $result_array_count (i32.add (local.get $count_0) (local.get $count_1)))

    (call $gc_if_out_of_space (i32.add (global.get $BYTE_ARRAY_HEADER_BYTE_SIZE) (local.get $result_array_count)))
    (local.set $result_pointer (global.get $FREE))

    ;; ==header==

    ;; 1 byte for garbage collection liveness.
    (i32.store8 (global.get $FREE) (global.get $GC_TAG_LIVE))
    (call $inc_free (global.get $GC_TAG_BYTE_SIZE))
    ;; 1 byte tag (used by gc going from grey ~> black)
    (i32.store8 (global.get $FREE) (global.get $BYTE_ARRAY_TAG))
    (call $inc_free (global.get $TAG_BYTE_SIZE))
    ;; 4 byte count 
    (i32.store (global.get $FREE) (local.get $result_array_count))
    (call $inc_free (global.get $BYTE_ARRAY_COUNT_SIZE))

    ;; ==contents==
    (memory.copy (global.get $FREE) (local.get $raw_pointer_0) (local.get $count_0))
    (call $inc_free (local.get $count_0))
    (memory.copy (global.get $FREE) (local.get $raw_pointer_1) (local.get $count_1))
    (call $inc_free (local.get $count_1))

    (local.get $result_pointer)
  )

  ;;   stack = [..., slice0, slice1]
  ;; ~>
  ;;   stack = [..., concat_slice_to_newly_allocated_array]
  (func $concat_slices
    (local $raw_pointer_0 i32)
    (local $count_0 i32)
    (local $raw_pointer_1 i32)
    (local $count_1 i32)

    (local $new_array_pointer i32)

    (call $dec_stack (i32.const 4))
    (local.set $raw_pointer_1 (i32.load (i32.add (i32.load (global.get $STACK)) (global.get $SLICE_POINTER_OFFSET))))
    (local.set $count_1 (i32.load (i32.add (i32.load (global.get $STACK)) (global.get $SLICE_COUNT_OFFSET))))
    (call $dec_stack (i32.const 1))
    
    (call $dec_stack (i32.const 4))
    (local.set $raw_pointer_0 (i32.load (i32.add (i32.load (global.get $STACK)) (global.get $SLICE_POINTER_OFFSET))))
    (local.set $count_0 (i32.load (i32.add (i32.load (global.get $STACK)) (global.get $SLICE_COUNT_OFFSET))))
    (call $dec_stack (i32.const 1))

    (local.set $new_array_pointer (call $concat_arrays (local.get $raw_pointer_0) (local.get $count_0) (local.get $raw_pointer_1) (local.get $count_1)))

    (call $array_slice
          (local.get $new_array_pointer)
          (i32.add (local.get $new_array_pointer) (global.get $BYTE_ARRAY_CONTENTS_OFFSET))
          (i32.load (i32.add (local.get $new_array_pointer) (global.get $BYTE_ARRAY_COUNT_OFFSET))))
  )
  (export "concat_slices" (func $concat_slices))

  (; =====Garbage Collector===== ;)
  (func $gc_if_out_of_space (param $allocation_request_byte_size i32)
    (if (i32.lt_u
          (i32.add (global.get $FREE) (local.get $allocation_request_byte_size))
          (i32.add (global.get $A_HEAP_START) (global.get $HEAP_SIZE)))
      (then)
      (else
        (call $gc)
        ;; Checking that gc helped.
        (if (i32.lt_u
              (i32.add (global.get $FREE) (local.get $allocation_request_byte_size))
              (i32.add (global.get $A_HEAP_START) (global.get $HEAP_SIZE)))
          (then)
          (else (call $on_out_of_memory)))))
  )

  ;; A := To Space
  ;; B := From Space
  ;; p a raw pointer to somewhere in A
  ;; q a raw pointer to somewhere in B
  (func $gc
    (local $tmp i32)

    (call $on_garbage_collection)

    (global.set $FREE (global.get $B_HEAP_START))
    (call $gc_walk_stack)
    (call $gc_traverse_grey)

    
    ;; swapping heaps
    (local.set $tmp (global.get $A_HEAP_START))
    (global.set $A_HEAP_START (global.get $B_HEAP_START))
    (global.set $B_HEAP_START (local.get $tmp))
  )
  (export "gc" (func $gc))

  (func $gc_walk_stack
    (local $current_element i32)
    (local $tag i32)
    (local $moved_to i32)

    (local.set $current_element (global.get $STACK_START))

    (block $end_loop
    (loop $main_loop
      (i32.eq (local.get $current_element) (global.get $STACK))
      (br_if $end_loop)

      (local.set $tag (i32.load8_u (local.get $current_element)))
      (if (i32.eq (local.get $tag) (global.get $CONST_TAG))
      (then
        ;; ==const==
        ;; skip
        (local.set $current_element (i32.add (local.get $current_element) (global.get $TAGGED_POINTER_BYTE_SIZE)))
      )
      (else (if (i32.eq (local.get $tag) (global.get $TUPLE_TAG))
      (then
        ;; ==tuple pointer==
        ;; move the tuple to B, and update the current tagged pointer in the stack
        (local.set $moved_to (call $gc_move_tuple (i32.load (i32.add (local.get $current_element) (i32.const 1)))))
        (i32.store
          (i32.add (local.get $current_element) (i32.const 1))
          (local.get $moved_to))

        (local.set $current_element (i32.add (local.get $current_element) (global.get $TAGGED_POINTER_BYTE_SIZE)))
      )
      (else (if (i32.eq (local.get $tag) (global.get $BYTE_ARRAY_SLICE_TAG))
      (then
        ;; ==slice pointer==
        (local.set $moved_to (call $gc_move_byte_array_slice (i32.load (i32.add (local.get $current_element) (i32.const 1)))))
        (i32.store
          (i32.add (local.get $current_element) (i32.const 1))
          (local.get $moved_to))

        (local.set $current_element (i32.add (local.get $current_element) (global.get $TAGGED_POINTER_BYTE_SIZE)))
      )
      (else (if (i32.eq (local.get $tag) (global.get $ENV_TAG))
      (then
        ;; ==env==
        ;; skip header
        (local.set $current_element (i32.add (local.get $current_element) (global.get $ENV_HEADER_BYTE_SIZE)))
      )
      (else
        ;; unknown tag
        unreachable))))))))
      (br $main_loop)))

  )
  (export "gc_walk_stack" (func $gc_walk_stack))

  (func $gc_traverse_grey
    (local $current_grey i32)
    (local $current_grey_tag i32)
    (local $next_grey i32)

    (local $tuple_components_count i32)
    (local $component_tag i32)

    (local $slice_parent_pointer i32)
    (local $new_slice_parent_pointer i32)
    (local $new_slice_pointer i32)

    (local.set $current_grey (global.get $B_HEAP_START))

    (block $end_loop
    (loop $main_loop
      (i32.eq (local.get $current_grey) (global.get $FREE))
      (br_if $end_loop)

      (local.set $current_grey_tag (i32.load8_u (i32.add (local.get $current_grey) (i32.const 1))))
      (if (i32.eq (local.get $current_grey_tag) (global.get $TUPLE_TAG))
      (then
        ;; ==Tuple==
        ;; skip the tuple header
        (local.set $tuple_components_count (i32.load8_u (i32.add (local.get $current_grey) (global.get $TUPLE_COUNT_OFFSET))))
        (local.set $current_grey (i32.add (local.get $current_grey) (global.get $TUPLE_HEADER_OFFSET)))
        (local.set $next_grey
          (i32.add
            (local.get $current_grey)
            (i32.mul (local.get $tuple_components_count) (global.get $TAGGED_POINTER_BYTE_SIZE))))

        (block $end_components_loop
        (loop $components_loop
          (i32.eq (local.get $current_grey) (local.get $next_grey))
          (br_if $end_components_loop)

          (local.set $component_tag (i32.load8_u (local.get $current_grey)))
          (if (i32.eq (local.get $component_tag) (global.get $CONST_TAG))
          (then
            ;; ==const==
            ;; skip
            (local.set $current_grey (i32.add (local.get $current_grey) (global.get $TAGGED_POINTER_BYTE_SIZE)))
          )
          (else (if (i32.eq (local.get $component_tag) (global.get $TUPLE_TAG))
          (then
            ;; ==tuple pointer==
            ;; move the tuple to B, and update the current tagged pointer
            (i32.store
              (i32.add (local.get $current_grey) (global.get $TAG_BYTE_SIZE))
              (call $gc_move_tuple (i32.load (i32.add (local.get $current_grey) (global.get $TAG_BYTE_SIZE)))))
            (local.set $current_grey (i32.add (local.get $current_grey) (global.get $TAGGED_POINTER_BYTE_SIZE)))
          )
          (else (if (i32.eq (local.get $component_tag) (global.get $BYTE_ARRAY_SLICE_TAG))
          (then
            ;; ==slice pointer==
            ;; move the slice to B, and update the current tagged pointer
            (i32.store
              (i32.add (local.get $current_grey) (global.get $TAG_BYTE_SIZE))
              (call $gc_move_byte_array_slice (i32.load (i32.add (local.get $current_grey) (global.get $TAG_BYTE_SIZE)))))
            (local.set $current_grey (i32.add (local.get $current_grey) (global.get $TAGGED_POINTER_BYTE_SIZE)))
          )
          (else
            ;; unknown tag
            unreachable))))))

          (br $components_loop)
        ))
      )
      (else (if (i32.eq (local.get $current_grey_tag) (global.get $BYTE_ARRAY_SLICE_TAG))
      (then
        ;; ==Byte Array Slice==
        (local.set $slice_parent_pointer (i32.load (i32.add (local.get $current_grey) (global.get $SLICE_PARENT_POINTER_OFFSET))))

        ;; Update the two pointers. Note that we don't need to update slice_count.
        (local.set $new_slice_parent_pointer (call $gc_move_byte_array (local.get $slice_parent_pointer)))
        (local.set $new_slice_pointer
          (i32.add (local.get $new_slice_parent_pointer)
                   (i32.sub (i32.load (i32.add (local.get $current_grey) (global.get $SLICE_POINTER_OFFSET))) ;; slice_pointer
                            (local.get $slice_parent_pointer))))

        (i32.store
          (i32.add (local.get $current_grey) (global.get $SLICE_PARENT_POINTER_OFFSET))
          (local.get $new_slice_parent_pointer))

        (i32.store (i32.add (local.get $current_grey) (global.get $SLICE_POINTER_OFFSET)) (local.get $new_slice_pointer))

        (local.set $current_grey (i32.add (local.get $current_grey) (global.get $SLICE_BYTE_SIZE)))
      )
      (else (if (i32.eq (local.get $current_grey_tag) (global.get $BYTE_ARRAY_TAG))
      (then
        ;; ==Byte Array==
        ;; skip
        (local.set $current_grey
          (i32.add (local.get $current_grey)
          (i32.add (global.get $BYTE_ARRAY_HEADER_BYTE_SIZE)
                   (i32.load (i32.add (local.get $current_grey) (global.get $BYTE_ARRAY_COUNT_OFFSET))))))
      )
      (else
        unreachable))))))

      (br $main_loop)
    ))
  )
  (export "gc_traverse_grey" (func $gc_traverse_grey))

  ;; p is a raw pointer that points to a start of a tuple in A_SPACE.
  ;; q is a raw pointer that points to the new location of the tuple in B_SPACE
  ;; returns q
  (func $gc_move_tuple (param $p i32) (result i32)
    (local $q i32)
    (local $tuple_byte_size i32)

    (if (i32.eq (i32.load8_u (local.get $p)) (global.get $GC_TAG_MOVED))
      (then ;; moved
        (local.set $q (i32.load (i32.add (local.get $p) (i32.const 1)))))
      (else ;; live
        (local.set $q (global.get $FREE))
        ;; copy the tuple at p to q
        (local.set $tuple_byte_size
          (i32.add
              (global.get $TUPLE_HEADER_BYTE_SIZE)
              (i32.mul
                (i32.load8_u (i32.add (local.get $p) (global.get $TUPLE_COUNT_OFFSET)))
                (global.get $TAGGED_POINTER_BYTE_SIZE))))

        (memory.copy (local.get $q) (local.get $p) (local.get $tuple_byte_size))

        ;; update B_SPACE pointer
        (global.set $FREE (i32.add (global.get $FREE) (local.get $tuple_byte_size)))

        ;; put tombstone at p pointing to q
        (i32.store8 (local.get $p) (global.get $GC_TAG_MOVED))
        (i32.store (i32.add (local.get $p) (i32.const 1)) (local.get $q))
      ))

    (local.get $q)
  )
  (export "gc_move_tuple" (func $gc_move_tuple))

  ;; p is a raw pointer that points to a start of a string in A_SPACE (or possibly in STATIC).
  ;; q is a raw pointer that points to the new location of the tuple in B_SPAC (or possibly to p again if it is in STATIC)
  ;; returns q
  (func $gc_move_byte_array (param $p i32) (result i32)
    (local $q i32)
    (local $byte_array_size i32)

    (if (i32.eq (i32.load8_u (local.get $p)) (global.get $GC_TAG_MOVED))
      (then ;; moved
        (local.set $q (i32.load (i32.add (local.get $p) (i32.const 1)))))
      (else ;; live
        (if (i32.lt_u (local.get $p) (global.get $STACK_START)) ;; if p in STATIC
          (then ;; static
            (local.set $q (local.get $p))
          )
          (else ;; on the heap
            (local.set $q (global.get $FREE))
            (local.set $byte_array_size
              (i32.add (global.get $BYTE_ARRAY_HEADER_BYTE_SIZE)
                       (i32.load (i32.add (local.get $p) (global.get $BYTE_ARRAY_COUNT_OFFSET)))))
            (memory.copy (local.get $q) (local.get $p) (local.get $byte_array_size))

            ;; update B_SPACE pointer
            (global.set $FREE (i32.add (global.get $FREE) (local.get $byte_array_size)))

            ;; put tombstone at p pointing to q
            (i32.store8 (local.get $p) (global.get $GC_TAG_MOVED))
            (i32.store (i32.add (local.get $p) (i32.const 1)) (local.get $q))
          )
        )
      )
    )

    (local.get $q)
  )
  (export "gc_move_byte_array" (func $gc_move_byte_array))

  (func $gc_move_byte_array_slice (param $p i32) (result i32)
    (local $q i32)

    (if (i32.eq (i32.load8_u (local.get $p)) (global.get $GC_TAG_MOVED))
      (then ;; moved
        (local.set $q (i32.load (i32.add (local.get $p) (i32.const 1)))))
      (else ;; live
        (local.set $q (global.get $FREE))
        (memory.copy (local.get $q) (local.get $p) (global.get $SLICE_BYTE_SIZE))

        ;; update B_SPACE pointer
        (global.set $FREE (i32.add (global.get $FREE) (global.get $SLICE_BYTE_SIZE)))

        ;; put tombstone at p pointing to q
        (i32.store8 (local.get $p) (global.get $GC_TAG_MOVED))
        (i32.store (i32.add (local.get $p) (i32.const 1)) (local.get $q))
      )
    )

    (local.get $q)
  )
  (export "gc_move_byte_array_slice" (func $gc_move_byte_array_slice))
  
)
