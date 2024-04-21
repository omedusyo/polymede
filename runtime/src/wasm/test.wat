(module
  (func $square (param $x i32) (result i32)
    (i32.mul (local.get $x) (local.get $x))
  )

  (export "f" (func $square))
)
