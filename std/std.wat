(module
  ;; Default memory definition
  (memory $memory 1)
  (export "memory" (memory $memory))

  (global $__metro_allocOffset (mut i32) (i32.const 1024))

  (func $i32_store (param $location i32) (param $value i32) (result i32)
    (i32.store (get_local $location) (get_local $value))
    (get_local $location)
  )

  ;; Allocate heap space
  (func $__metro_alloc (param $numberOfBytes i32) (result i32)
    (local $offset i32)
    (set_local $offset (get_global $__metro_allocOffset))
    (set_global $__metro_allocOffset (i32.add (get_local $offset) (get_local $numberOfBytes)))
    (get_local $offset)
  )

  ;; Start here
  (start $main)
)
