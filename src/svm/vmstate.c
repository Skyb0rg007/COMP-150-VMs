// Memory management and literal addition for VMState

// You'll complete this file as part of module 1


#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "vmstate.h"
#include "vtable.h"
#include "value.h"

void freestatep(VMState *sp) {
    assert(sp && *sp);
    VMState vm = *sp;
    vector_free(&vm->literals);
    free(vm);
    *sp = NULL;
}

VMState newstate(void) {
    VMState vm = malloc(sizeof *vm);
    assert(vm != NULL);
    for (int i = 0; i < NUM_REGISTERS; i++) {
        vm->registers[i] = nilValue;
    }
    vm->globals = VTable_new(HINT_NUM_GLOBALS);
    vector_init(&vm->literals);
    return vm;
}

int literal_slot(VMState vm, Value literal) {
    Value *x = vector_emplace_back(&vm->literals);
    assert(x != NULL);
    *x = literal;
    return vector_size(&vm->literals) - 1;
}

int literal_count(VMState vm) {
    return vector_size(&vm->literals);
}
