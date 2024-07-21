(; memory.copy $destination $source $size ;)

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

  (import "runtime" "add" (func $add))
  (import "runtime" "inc" (func $inc))
  (import "runtime" "dec" (func $dec))

  (import "console" "log" (func $log (param i32)))
  (import "console" "logStack" (func $log_stack))
  (import "console" "logHeap" (func $log_heap))

  (table 50 funcref)


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
        (call $copy_and_extend_env (i32.const 1)) ;; let m := n - 1
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

    (call $copy_and_extend_env (i32.const 3))
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

  (func $range_test_0 (param $count i32)
    (call $const (local.get $count))
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
