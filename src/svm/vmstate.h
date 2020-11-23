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
/* Can't include "vmerror.h" because it includes this file */
extern noreturn void runerror(VMState state, const char *format, ...);

/* Number of VM registers */
#define NUM_REGISTERS 500
/* Number of activation records */
#define NUM_ACTIVATIONS 1000
/* Hint for initial size of the globals table */
#define HINT_NUM_GLOBALS 20

typedef struct VMState *VMState;

/* XXX: Reorder fields and remove extra useless comments */
struct VMState {
    /* Global variables */
    VTable_T globals;
    /* The literal pool */
    vector(Value) literals;
    struct VMFunction *current_fun;
    int current_instrnum;
    /* VM registers */
    Value registers[NUM_REGISTERS];
    /* Activation records */
    int num_activations;
    struct Activation activations[NUM_ACTIVATIONS];
    /* XXX: offset -> pointer */
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
    /* XXX: Remove when debug is done */
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

static inline void vmstate_store_act(VMState vm, Instruction *ip, int destreg) {
    struct Activation *a = &vm->activations[vm->num_activations++];
    if (vm->num_activations > NUM_ACTIVATIONS)
        runerror(vm, "Stack overflow!");
    a->ip = ip;
    a->destreg = destreg;
    a->window = vm->window;
}

static inline int vmstate_restore_act(VMState vm, Instruction **ip) {
    struct Activation *a = &vm->activations[--vm->num_activations];
    if (vm->num_activations < 0)
        runerror(vm, "Stack underflow!");
    *ip = a->ip;
    vm->window = a->window;
    return a->destreg;
}

int literal_count(VMState state);
  // Returns N, the number of index values for which it
  // is ok to call `literal_value` (range 0 to N-1)

#endif /* VMSTATE_INCLUDED */
