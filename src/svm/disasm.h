//// Code for disassembling an instructions, showing values

// Optional, but *very* useful.  I recommend you call
// `svmdebug_value("decode")` at the start of your `vmrun` function,
// and if the result you get back is not NULL, call `idump`
// before decoding each instruction.

#ifndef DISASM_H
#define DISASM_H

#include <stdio.h>

#include "iformat.h"
#include "vmstate.h"

//// simple printer, emits an approximation to assembly code

void printasm(FILE *fp, VMState vm, Instruction i);

void idump(FILE *fp, VMState vm, int pc, Instruction I, 
//@ module >= 7
           long int regbase,
//@ true
           Value *RX, Value *RY, Value *RZ);


#endif


