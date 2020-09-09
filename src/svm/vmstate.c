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
    free(vm);
    *sp = NULL;
}

VMState newstate(void) {
    VMState vm = malloc(sizeof *vm);
    assert(vm != NULL);
    vm->ip = NULL;
    for (int i = 0; i < NUM_REGISTERS; i++) {
        vm->registers[i] = nilValue;
    }
    vm->globals = VTable_new(HINT_NUM_GLOBALS);
    for (int i = 0; i < NUM_LITERALS; i++) {
        vm->literals[i] = nilValue;
    }
    vm->num_literals = 0;
    return vm;
}

int literal_slot(VMState state, Value literal) {
    assert(state->num_literals < NUM_LITERALS);
    state->literals[state->num_literals] = literal;
    return state->num_literals++;
}

