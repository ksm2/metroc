#ifndef RUNTIME_H
#define RUNTIME_H

#include <wasm.h>
#include <wasmtime.h>

wasm_engine_t *create_engine();
void delete_engine(wasm_engine_t *engine);
wasm_store_t *create_store(wasm_engine_t *engine);
void delete_store(wasm_store_t *store);
wasm_module_t *create_module(wasm_engine_t *engine, const char* filename);
void delete_module(wasm_module_t *module);
wasmtime_linker_t *create_linker(wasm_store_t *store);
void delete_linker(wasmtime_linker_t *linker);
wasm_instance_t *create_instance(wasmtime_linker_t *linker, const wasm_module_t *module);
void delete_instance(wasm_instance_t *instance);
void link_wasi(wasm_store_t *store, wasmtime_linker_t *linker);
void call_func(const wasm_instance_t *instance, int func_index);
int find_func_index(const wasm_module_t *module, const char *expected_name);

#endif
