// State of a VM, and functions to allocate, deallocate, add a literal

// This one's the essential part of module 1.
// You'll define the key representation, `struct VMState`,
// and you'll use it in your `vmrun` function.

#ifndef VMSTATE_INCLUDED
#define VMSTATE_INCLUDED

#include <stdint.h>

#include "value.h"
#include "vtable.h"

#define NUM_REGISTERS 8
#define NUM_LITERALS  (1 << 5)
#define STORE_SIZE    (1 << 10)
#define HINT_NUM_GLOBALS 20

typedef struct VMState *VMState;

struct VMState {
    Instruction *ip;
    Value registers[NUM_REGISTERS];
    VTable_T globals;
    Value literals[NUM_LITERALS];
    uint32_t num_literals;
};

VMState newstate(void);       // allocate and initialize (to empty)
void freestatep(VMState *sp); // deallocate

int literal_slot(VMState state, Value literal);
  // return index of literal in `literals`, adding if needed
  // (at need, can be postponed to module 2)

#endif /* VMSTATE_INCLUDED */
