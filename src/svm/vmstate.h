// State of a VM, and functions to allocate, deallocate, add a literal

// This one's the essential part of module 1.
// You'll define the key representation, `struct VMState`,
// and you'll use it in your `vmrun` function.

#ifndef VMSTATE_INCLUDED
#define VMSTATE_INCLUDED

#include <assert.h>
#include <stdint.h>

#include "value.h"
#include "vtable.h"
#include "utils/vector.h"
#include "vmstack.h"
#include "vmerror.h"

/* Number of VM registers */
#define NUM_REGISTERS 500
/* Number of activation records */
#define NUM_ACTIVATIONS 1000
/* Hint for initial size of the globals table */
#define HINT_NUM_GLOBALS 20

typedef struct VMState *VMState;

struct VMState {
    /* Global variables */
    VTable_T globals;
    /* The literal pool */
    vector(Value) literals;
    int num_activations;
    /* VM registers */
    Value registers[NUM_REGISTERS];
    /* Activation records */
    struct Activation activations[NUM_ACTIVATIONS];
    int window;
};

VMState newstate(void);       // allocate and initialize (to empty)
void freestatep(VMState *sp); // deallocate

int literal_slot(VMState state, Value literal);
  // return index of literal in `literals`, adding if needed
  // (at need, can be postponed to module 2)

#define literal_value(vm, index) vmstate_get_lit(vm, index)

static inline Value vmstate_get_lit(VMState vm, uint16_t index)
{
    assert(index < vector_size(&vm->literals));
    return vector_at(&vm->literals, index);
}

static inline Value vmstate_get_reg(VMState vm, int index)
{
    index += vm->window;
    if (index >= NUM_REGISTERS)
        runerror(vm, "Window broken!");
    return vm->registers[index];
}

static inline void vmstate_set_reg(VMState vm, int index, Value x)
{
    index += vm->window;
    if (index >= NUM_REGISTERS)
        runerror(vm, "Window broken!");
    vm->registers[index] = x;
}

int literal_count(VMState state);
  // Returns N, the number of index values for which it
  // is ok to call `literal_value` (range 0 to N-1)

#endif /* VMSTATE_INCLUDED */
