// Heart of the VM: runs instructions until told to halt

// You'll write a small `vmrun` function in module 1.  You'll pay
// some attention to performance, but you'll implement only a few 
// instructions.  You'll add other instructions as needed in future modules.

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "check-expect.h"
#include "iformat.h"
#include "value.h"
#include "vmstate.h"
#include "vmrun.h"

#include "print.h"

#include "vmerror.h"
#include "vmheap.h"
#include "vmstring.h"
#include "vtable.h"

void vmrun(VMState vm, struct VMFunction *fun) {
  (void) vm;
  (void) fun;

  Instruction *ip = fun->instructions;

  while (true) {
      Instruction i = *ip++;
      Opcode op = opcode(i);
      switch (op)
      {
          case Halt:
              return;
          case Print: {
              uint8_t reg_num = uX(i);
              assert(reg_num < NUM_REGISTERS);
              print("%v\n", vm->registers[reg_num]);
              break;
          }
          case Check: {
              uint8_t reg_num = uX(i);
              uint16_t lit_idx = uYZ(i);
              assert(reg_num < NUM_REGISTERS);
              assert(lit_idx < vm->num_literals);
              assert(vm->literals[lit_idx].tag == String);
              check(vm->literals[lit_idx].s->bytes, vm->registers[reg_num]);
              break;
          }
          case Expect: {
              uint8_t reg_num = uX(i);
              uint16_t lit_idx = uYZ(i);
              assert(reg_num < NUM_REGISTERS);
              assert(lit_idx < vm->num_literals);
              assert(vm->literals[lit_idx].tag == String);
              expect(vm->literals[lit_idx].s->bytes, vm->registers[reg_num]);
              break;
          }
          default:
              printf("Opcode not implemented\n");
              abort();
      }
  }
}

