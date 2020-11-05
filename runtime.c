/*
Example of compiling, instantiating, and linking two WebAssembly modules
together.

You can compile and run this example on Linux with:

   cargo build --release -p wasmtime-c-api
   cc examples/linking.c \
       -I crates/c-api/include \
       -I crates/c-api/wasm-c-api/include \
       target/release/libwasmtime.a \
       -lpthread -ldl -lm \
       -o linking
   ./linking

Note that on Windows and macOS the command will be similar, but you'll need
to tweak the `-lpthread` and such annotations.
*/

#ifdef _WIN32
//#error The VS ASM compiler won't work with this, but you can get external ones to do the trick
#define BINDATA #error BINDATA requires nasm
#else

__asm__(
".altmacro\n" \
".macro binfile p q\n" \
"   .global \\p\n" \
"\\p:\n" \
"   .incbin \\q\n" \
"\\p&_end:\n" \
"   .byte 0\n" \
"   .global \\p&_len\n" \
"\\p&_len:\n" \
"   .int(\\p&_end - \\p)\n" \
".endm\n\t"
);

#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <wasm.h>
#include <wasi.h>
#include <wasmtime.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))

static void exit_with_error(const char *message, wasmtime_error_t *error, wasm_trap_t *trap);

#define xstr(s) str(s)
#define str(s) #s

#define WASM_ASM_CODE "\n\n.data\n\tbinfile wat_file \"" xstr(WASM_FILE) "\"\n"
__asm__(WASM_ASM_CODE);
extern wasm_byte_t wat_file;
extern int wat_file_len;

int main() {
  int ret = 0;
  // Set up our context
  wasm_engine_t *engine = wasm_engine_new();
  assert(engine != NULL);
  wasm_store_t *store = wasm_store_new(engine);
  assert(store != NULL);

  wasm_byte_vec_t metro_wasm;
  wasm_byte_vec_new(&metro_wasm, wat_file_len, &wat_file);

  // Compile our two modules
  wasmtime_error_t *error;
  wasm_module_t *metro_module = NULL;
  error = wasmtime_module_new(engine, &metro_wasm, &metro_module);
  if (error != NULL)
    exit_with_error("failed to compile metro", error, NULL);
  wasm_byte_vec_delete(&metro_wasm);

  // Instantiate wasi
  wasi_config_t *wasi_config = wasi_config_new();
  assert(wasi_config);
  wasi_config_inherit_argv(wasi_config);
  wasi_config_inherit_env(wasi_config);
  wasi_config_inherit_stdin(wasi_config);
  wasi_config_inherit_stdout(wasi_config);
  wasi_config_inherit_stderr(wasi_config);
  wasm_trap_t *trap = NULL;
  wasi_instance_t *wasi = wasi_instance_new(store, "wasi_unstable", wasi_config, &trap);
  if (wasi == NULL) {
    exit_with_error("failed to instantiate wasi", NULL, trap);
  }

  // Create our linker which will be linking our modules together, and then add
  // our WASI instance to it.
  wasmtime_linker_t *linker = wasmtime_linker_new(store);
  error = wasmtime_linker_define_wasi(linker, wasi);
  if (error != NULL)
    exit_with_error("failed to link wasi", error, NULL);

  // Instantiate `metro` with the linker
  wasm_instance_t *metro;
  error = wasmtime_linker_instantiate(linker, metro_module, &metro, &trap);
  if (error != NULL || trap != NULL)
    exit_with_error("failed to instantiate metro", error, trap);

  // Lookup our `main` export function
  wasm_extern_vec_t metro_externs;
  wasm_instance_exports(metro, &metro_externs);
  assert(metro_externs.size == 2);
  wasm_func_t *main = wasm_extern_as_func(metro_externs.data[1]);
  assert(main != NULL);
  error = wasmtime_func_call(main, NULL, 0, NULL, 0, &trap);
  if (error != NULL || trap != NULL)
    exit_with_error("failed to call main", error, trap);

  // Clean up after ourselves at this point
  wasm_extern_vec_delete(&metro_externs);
  wasm_instance_delete(metro);
  wasmtime_linker_delete(linker);
  wasm_module_delete(metro_module);
  wasm_store_delete(store);
  wasm_engine_delete(engine);
  return 0;
}

static void read_wasm_file(
  wasm_engine_t *engine,
  wasm_byte_vec_t *bytes
) {
//  wasm_byte_vec_t wat;
  wasm_byte_vec_new(bytes, wat_file_len, &wat_file);
  // Parse the wat into the binary wasm format
//  wasmtime_error_t *error = wasmtime_wat2wasm(&wat, bytes);
//  if (error != NULL) {
//    exit_with_error("failed to parse wat", error, NULL);
//  }
//  wasm_byte_vec_delete(&wat);
}

static void exit_with_error(const char *message, wasmtime_error_t *error, wasm_trap_t *trap) {
  fprintf(stderr, "error: %s\n", message);
  wasm_byte_vec_t error_message;
  if (error != NULL) {
    wasmtime_error_message(error, &error_message);
    wasmtime_error_delete(error);
  } else {
    wasm_trap_message(trap, &error_message);
    wasm_trap_delete(trap);
  }
  fprintf(stderr, "%.*s\n", (int) error_message.size, error_message.data);
  wasm_byte_vec_delete(&error_message);
  exit(1);
}
