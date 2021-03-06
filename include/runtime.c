#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <wasm.h>
#include <wasi.h>
#include <wasmtime.h>

static bool wasm_name_equals(const wasm_name_t *name, const char *expected_name);
static void exit_with_error(const char *message, wasmtime_error_t *error, wasm_trap_t *trap);
static void wat_byte_vec_to_wasm(wasm_byte_vec_t *bytes, const wasm_byte_vec_t *wat);

wasm_engine_t *create_engine() {
  wasm_engine_t *engine = wasm_engine_new();
  assert(engine != NULL);
  return engine;
}

void delete_engine(wasm_engine_t *engine) {
  wasm_engine_delete(engine);
}

wasm_store_t *create_store(wasm_engine_t *engine) {
  wasm_store_t *store = wasm_store_new(engine);
  assert(store != NULL);
  return store;
}

void delete_store(wasm_store_t *store) {
  wasm_store_delete(store);
}

wasm_module_t *create_module(wasm_engine_t *engine, size_t size, const wasm_byte_t *wat_bytes) {
  wasm_byte_vec_t byte_vec;
  wasm_byte_vec_t wat;

  wasm_byte_vec_new(&wat, size, wat_bytes);
  wat_byte_vec_to_wasm(&byte_vec, &wat);
  wasm_byte_vec_delete(&wat);

  wasmtime_error_t *error;
  wasm_module_t *module = NULL;
  error = wasmtime_module_new(engine, &byte_vec, &module);
  if (error != NULL)
    exit_with_error("failed to compile linking", error, NULL);
  wasm_byte_vec_delete(&byte_vec);

  return module;
}

void delete_module(wasm_module_t *module) {
  wasm_module_delete(module);
}

wasmtime_linker_t *create_linker(wasm_store_t *store) {
  return wasmtime_linker_new(store);
}

void delete_linker(wasmtime_linker_t *linker) {
  wasmtime_linker_delete(linker);
}

wasm_instance_t *create_instance(wasmtime_linker_t *linker, const wasm_module_t *module) {
  wasmtime_error_t *error;
  wasm_instance_t *instance;
  wasm_trap_t *trap = NULL;

  error = wasmtime_linker_instantiate(linker, module, &instance, &trap);
  if (error != NULL || trap != NULL)
    exit_with_error("failed to instantiate instance", error, trap);

  return instance;
}

void delete_instance(wasm_instance_t *instance) {
  wasm_instance_delete(instance);
}

void call_func(const wasm_instance_t *instance, int func_index) {
  wasmtime_error_t *error;
  wasm_trap_t *trap = NULL;
  wasm_extern_vec_t instance_externs;
  wasm_instance_exports(instance, &instance_externs);

  wasm_func_t *main_fn = wasm_extern_as_func(instance_externs.data[func_index]);
  assert(main_fn != NULL);

  error = wasmtime_func_call(main_fn, NULL, 0, NULL, 0, &trap);
  if (error != NULL || trap != NULL) {
    exit_with_error("failed to call func", error, trap);
  }
}

wasm_byte_t *call_func_with_error(const wasm_instance_t *instance, int func_index, size_t *error_size) {
  wasmtime_error_t *error;
  wasm_trap_t *trap = NULL;
  wasm_extern_vec_t instance_externs;
  wasm_instance_exports(instance, &instance_externs);

  wasm_func_t *main_fn = wasm_extern_as_func(instance_externs.data[func_index]);
  assert(main_fn != NULL);

  error = wasmtime_func_call(main_fn, NULL, 0, NULL, 0, &trap);
  if (trap == NULL) {
    assert(error == NULL);
    return NULL;
  }

  wasm_message_t message;
  wasm_trap_message(trap, &message);

  // Return
  *error_size = message.size;
  return message.data;
}

int find_func_index(const wasm_module_t *module, const char *expected_name) {
  wasm_exporttype_vec_t exports;
  wasm_module_exports(module, &exports);

  for (int i = 0; i < exports.size; ++i) {
    const wasm_name_t *name = wasm_exporttype_name(exports.data[i]);
    if (wasm_name_equals(name, expected_name)) {
      return i;
    }
  }

  return -1;
}

wasm_byte_t *wat_to_wasm(size_t wat_size, const wasm_byte_t *wat_bytes, size_t *wasm_bytes) {
  wasm_byte_vec_t byte_vec;
  wasm_byte_vec_t wat;

  wasm_byte_vec_new(&wat, wat_size, wat_bytes);
  wat_byte_vec_to_wasm(&byte_vec, &wat);
  wasm_byte_vec_delete(&wat);

  *wasm_bytes = byte_vec.size;
  return byte_vec.data;
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

static void wat_byte_vec_to_wasm(wasm_byte_vec_t *bytes, const wasm_byte_vec_t *wat) {
  // Parse the wat into the binary wasm format
  wasmtime_error_t *error = wasmtime_wat2wasm(wat, bytes);
  if (error != NULL)
    exit_with_error("failed to parse wat", error, NULL);
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
