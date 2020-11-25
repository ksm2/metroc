#ifndef RUNTIME_H
#define RUNTIME_H

#include <wasm.h>

wasm_engine_t *create_engine();
void delete_engine(wasm_engine_t *engine);
wasm_store_t *create_store(wasm_engine_t *engine);
void delete_store(wasm_store_t *store);
wasm_module_t *create_module(wasm_engine_t *engine, const char* filename);
void delete_module(wasm_module_t *module);
void run_wat_file(wasm_store_t *store, wasm_module_t *module);

#endif
