#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <wasm.h>
#include <wasi.h>
#include <wasmtime.h>

static int find_main(const wasm_module_t *linking_module);
static bool wasm_name_equals(const wasm_name_t *name, const char *expected_name);
static void exit_with_error(const char *message, wasmtime_error_t *error, wasm_trap_t *trap);
static void read_wat_file(wasm_engine_t *engine, wasm_byte_vec_t *bytes, const char *file);

void executeWasm(const char* fname) {
  // Set up our context
  wasm_engine_t *engine = wasm_engine_new();
  assert(engine != NULL);
  wasm_store_t *store = wasm_store_new(engine);
  assert(store != NULL);

  wasm_byte_vec_t linking_wasm;
  read_wat_file(engine, &linking_wasm, fname);

  // Compile our two modules
  wasmtime_error_t *error;
  wasm_module_t *linking_module = NULL;
  error = wasmtime_module_new(engine, &linking_wasm, &linking_module);
  if (error != NULL)
    exit_with_error("failed to compile linking", error, NULL);
  wasm_byte_vec_delete(&linking_wasm);

  // Find main
  int main_index = find_main(linking_module);
  if (main_index < 0) {
    fprintf(stderr, "error: %s\n", "cannot find main. Did you export it?");
    exit(1);
  }

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
  if (wasi == NULL)
    exit_with_error("failed to instantiate wasi", NULL, trap);

  // Create our linker which will be linking our modules together, and then add
  // our WASI instance to it.
  wasmtime_linker_t *linker = wasmtime_linker_new(store);
  error = wasmtime_linker_define_wasi(linker, wasi);
  if (error != NULL)
    exit_with_error("failed to link wasi", error, NULL);

  // Instantiate `linking` with our linker.
  wasm_instance_t *linking;
  error = wasmtime_linker_instantiate(linker, linking_module, &linking, &trap);
  if (error != NULL || trap != NULL)
    exit_with_error("failed to instantiate linking", error, trap);

  // Lookup our `run` export function
  wasm_extern_vec_t linking_externs;
  wasm_instance_exports(linking, &linking_externs);
  wasm_func_t *main_fn = wasm_extern_as_func(linking_externs.data[main_index]);
  assert(main_fn != NULL);
  error = wasmtime_func_call(main_fn, NULL, 0, NULL, 0, &trap);
  if (error != NULL || trap != NULL)
    exit_with_error("failed to call main", error, trap);

  // Clean up after ourselves at this point
  wasm_instance_delete(linking);
  wasmtime_linker_delete(linker);
  wasm_module_delete(linking_module);
  wasm_store_delete(store);
  wasm_engine_delete(engine);
}

static int find_main(const wasm_module_t *linking_module) {
  wasm_exporttype_vec_t exports;
  wasm_module_exports(linking_module, &exports);

  for (int i = 0; i < exports.size; ++i) {
    const wasm_name_t *name = wasm_exporttype_name(exports.data[i]);
    if (wasm_name_equals(name, "main")) {
      return i;
    }
  }

  return -1;
}

static bool wasm_name_equals(const wasm_name_t *name, const char *expected_name) {
  if (name->size != strlen(expected_name)) {
    return false;
  }

  for (int i = 0; i < name->size; ++i) {
    if (name->data[i] != expected_name[i]) {
      return false;
    }
  }

  return true;
}

static void read_wat_file(
  wasm_engine_t *engine,
  wasm_byte_vec_t *bytes,
  const char *filename
) {
  wasm_byte_vec_t wat;
  // Load our input file to parse it next
  FILE* file = fopen(filename, "r");
  if (!file) {
    printf("> Error loading file!\n");
    exit(1);
  }
  fseek(file, 0L, SEEK_END);
  size_t file_size = ftell(file);
  wasm_byte_vec_new_uninitialized(&wat, file_size);
  fseek(file, 0L, SEEK_SET);
  if (fread(wat.data, file_size, 1, file) != 1) {
    printf("> Error loading module!\n");
    exit(1);
  }
  fclose(file);

  // Parse the wat into the binary wasm format
  wasmtime_error_t *error = wasmtime_wat2wasm(&wat, bytes);
  if (error != NULL)
    exit_with_error("failed to parse wat", error, NULL);
  wasm_byte_vec_delete(&wat);
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
