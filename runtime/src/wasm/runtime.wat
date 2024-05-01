;; TODO: Change `get` to `take` - this will indicate that the operation actually consumes the value on the Linear Stack
;;       Also introduce `read` functions that will just copy stuff from the linear stack to WASM stack without consumption.

(module
  (memory 2 10)
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
  (export "tuple" (func $tuple))

  (; Assumes that a pointer to a tuple is on the top of the Linear Stack. ;)
  (; Moves its to WASM stack ;)
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
  (export "make_env" (func $make_env))

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
  (export "drop_env" (func $drop_env))

  (; ===Primitive Operations=== ;)
  (func $add
    (call $const (i32.add (call $get_const) (call $get_const)))
  )
  (export "add" (func $add))

  (func $inc
    (call $const (i32.add (call $get_const) (i32.const 1)))
  )
  (export "inc" (func $inc))

  (func $dec
    (call $const (i32.sub (call $get_const) (i32.const 1)))
  )
  (export "dec" (func $dec))
)

