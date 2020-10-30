(module
  ;; WASI imports
  (import "wasi_unstable" "fd_write" (func $wasi.fdWrite (param i32) (param i32) (param i32) (param i32) (result i32)))

  ;; Default memory definition
  (memory $memory 1)
  (export "memory" (memory $memory))

  (func $__metro_fd_write (param $handle i32) (param $string i32)
    (local $length i32)
    (set_local $length (i32.load (get_local $string)))
    (i32.store (i32.const 0) (i32.add (get_local $string) (i32.const 4)))
    (i32.store (i32.const 4) (get_local $length))
    (call $wasi.fdWrite (get_local $handle) (i32.const 0) (i32.const 1) (i32.const 20))
    drop
  )

  ;; Allocate heap space
  (func $__metro_alloc (param $numberOfBytes i32) (result i32)
    ;; TODO: Actually require some space on memory
    (i32.const 1024)
  )

  ;; Start here
  (start $main)
)
