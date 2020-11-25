#ifndef RUNTIME_H
#define RUNTIME_H

#include <wasm.h>

wasm_engine_t *create_engine();
void delete_engine(wasm_engine_t *engine);
void executeWasm(wasm_engine_t *engine, char* fname);

#endif
