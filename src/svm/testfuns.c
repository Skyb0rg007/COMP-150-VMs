// Builds a few functions for testing.  See description in testfuns.h

// Module 2: These functions could be used as a model for code 
// to run in your loader.

#include "testfuns.h"

#include "iformat.h"
#include "opcode.h"
#include "value.h"
#include "vmheap.h"
#include "vmstate.h"

static struct VMFunction *newfunction(int instruction_count) {
  // play it safe: always tack a Halt instruction into the end
  struct VMFunction *fun =
    vmalloc(sizeof (*fun) + (instruction_count + 1) * sizeof(fun->instructions[0]));
  fun->arity = 0;
  fun->size  = instruction_count + 1;
  fun->instructions[instruction_count] = eR0(Halt);
  return fun;
}


struct VMFunction *haltfun(void) {
  return newfunction(0); // nothing but the Halt instruction
}

struct VMFunction *print0fun(void) {
  struct VMFunction *fun = newfunction(1);
  fun->instructions[0] = eR1(Print, 0);
  return fun;
}

struct VMFunction *ce0fun(VMState vm) {
  VMString r0string = Vmstring_newc("$r0");
  int stringslot = literal_slot(vm, mkStringValue(r0string));
  struct VMFunction *fun = newfunction(2);
  fun->instructions[0] = eR1U16(Check, 0, stringslot);
  fun->instructions[1] = eR1U16(Expect, 0, stringslot);
  return fun;
}

