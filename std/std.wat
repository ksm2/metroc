(module
  ;; Default memory definition
  (memory $memory 1)
  (export "memory" (memory $memory))

  (global $__allocationOffset (mut i32) (i32.const 2056))

  ;; Stores a Byte at the given location
  (func $__storeByte (param $location i32) (param $value i32)
    (i32.store8 (get_local $location) (get_local $value))
  )

  ;; Loads a Byte at the given location
  (func $__loadByte (param $location i32) (result i32)
    (i32.load8_s (get_local $location))
  )

  ;; Stores an Int at the given location
  (func $__storeInt (param $location i32) (param $value i32)
    (i32.store (get_local $location) (get_local $value))
  )

  ;; Loads an Int at the given location
  (func $__loadInt (param $location i32) (result i32)
    (i32.load (get_local $location))
  )

  ;; Stores a Long at the given location
  (func $__storeLong (param $location i32) (param $value i64)
    (i64.store (get_local $location) (get_local $value))
  )

  ;; Loads a Long at the given location
  (func $__loadLong (param $location i32) (result i64)
    (i64.load (get_local $location))
  )

  ;; Allocate the given amount of bytes in heap space
  (func $__allocate (param $numberOfBytes i32) (result i32)
    (local $offset i32)
    (set_local $offset (get_global $__allocationOffset))
    (set_global $__allocationOffset (i32.add (get_local $offset) (get_local $numberOfBytes)))
    (get_local $offset)
  )

  ;; Copy $size bytes of memory from $source to $destination
  (func $__memcpy (param $destination i32) (param $source i32) (param $size i32)
    (block $___else_memcpy0
      (block $___if_memcpy1
        (br_if $___if_memcpy1 (i32.eqz (i32.ge_s (get_local $size) (i32.const 8))))
        (call $__storeLong (get_local $destination) (call $__loadLong (get_local $source)))
        (call $__memcpy (i32.add (get_local $destination) (i32.const 8)) (i32.add (get_local $source) (i32.const 8)) (i32.sub (get_local $size) (i32.const 8)))
        (br $___else_memcpy0)
      )
      (block $___if_memcpy2
        (br_if $___if_memcpy2 (i32.eqz (i32.eqz (i32.eq (get_local $size) (i32.const 0)))))
        (call $__storeByte (get_local $destination) (call $__loadByte (get_local $source)))
        (call $__memcpy (i32.add (get_local $destination) (i32.const 1)) (i32.add (get_local $source) (i32.const 1)) (i32.sub (get_local $size) (i32.const 1)))
      )
    )
  )

  ;; Concatnate two strings
  (func $__concat (param $str1 i32) (param $str2 i32) (result i32)
    (local $length1 i32)
    (local $length2 i32)
    (local $ptr i32)
    (set_local $length1 (call $__loadInt (get_local $str1)))
    (set_local $length2 (call $__loadInt (get_local $str2)))
    (set_local $ptr (call $__allocate (i32.add (i32.add (get_local $length1) (get_local $length2)) (i32.const 4))))
    (call $__storeInt (get_local $ptr) (i32.add (get_local $length1) (get_local $length2)))
    (call $__memcpy (i32.add (get_local $ptr) (i32.const 4)) (i32.add (get_local $str1) (i32.const 4)) (get_local $length1))
    (call $__memcpy (i32.add (i32.add (get_local $ptr) (get_local $length1)) (i32.const 4)) (i32.add (get_local $str2) (i32.const 4)) (get_local $length2))
    (get_local $ptr)
  )

  ;; Start here
  (start $main)
)
