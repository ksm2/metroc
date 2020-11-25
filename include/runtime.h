#ifndef RUNTIME_H
#define RUNTIME_H

#include <wasm.h>

wasm_engine_t *create_engine();
void delete_engine(wasm_engine_t *engine);
wasm_store_t *create_store(wasm_engine_t *engine);
void delete_store(wasm_store_t *store);
void run_wat_file(wasm_engine_t *engine, wasm_store_t *store, const char* fname);

#endif
