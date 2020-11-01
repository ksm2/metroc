(module
  ;; Default memory definition
  (memory $memory 1)
  (export "memory" (memory $memory))

  (global $__allocationOffset (mut i32) (i32.const 2056))

  ;; Stores an Int at the given location
  (func $__storeInt (param $location i32) (param $value i32)
    (i32.store (get_local $location) (get_local $value))
  )

  ;; Loads an Int at the given location
  (func $__loadInt (param $location i32) (result i32)
    (i32.load (get_local $location))
  )

  ;; Allocate the given amount of bytes in heap space
  (func $__allocate (param $numberOfBytes i32) (result i32)
    (local $offset i32)
    (set_local $offset (get_global $__allocationOffset))
    (set_global $__allocationOffset (i32.add (get_local $offset) (get_local $numberOfBytes)))
    (get_local $offset)
  )

  ;; Start here
  (start $main)
)
