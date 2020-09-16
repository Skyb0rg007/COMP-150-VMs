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
#include "value.h"

/* 'labels as values' is a GCC extension used for implementing 'vmrun'.
 * As a result, pedantic warnings must be disabled for this function.
 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
void vmrun(VMState vm, struct VMFunction *fun) {

    /* Points to the current instruction pointer */
    Instruction *ip = fun->instructions;
    /* The current instruction */
    Instruction i;
    /* The current opcode */
    Opcode op;
    /* Jump-table using the GCC 'labels as values' extension
     * Note that the Opcode type is an enum, so this table should
     * not have gaps.
     */
    static const void *dispatch_table[] = {
        [Halt] = &&do_halt,
        [Print] = &&do_print,
        [Check] = &&do_check,
        [Expect] = &&do_expect,
        [Add] = &&do_add,
        [LoadLiteral] = &&do_load_literal,
        [Goto] = &&do_goto,
        [If] = &&do_if
    };
    /* Increments the instruction pointer, jumping to the label in the jump table */
    #define DISPATCH() do {            \
        i = *ip++;                     \
        op = opcode(i);                \
        assert(op >= 0 && op < Unimp); \
        goto *dispatch_table[op];      \
    } while (0)

    DISPATCH();

    /* Implementation of each of the opcodes
     * Each section should end with a call to DISPATCH() to jump to the next
     * instruction's code
     */
    {
do_halt:
        return;
do_print:
        {
            uint8_t reg = uX(i);
            print("%v\n", vmstate_get_reg(vm, reg));
            DISPATCH();
        }
do_check:
        {
            uint8_t reg = uX(i);
            uint16_t lit = uYZ(i);
            check(AS_CSTRING(vm, vmstate_get_lit(vm, lit)), vmstate_get_reg(vm, reg));
            DISPATCH();
        }
do_expect:
        {
            uint8_t reg = uX(i);
            uint16_t lit = uYZ(i);
            expect(AS_CSTRING(vm, vmstate_get_lit(vm, lit)), vmstate_get_reg(vm, reg));
            DISPATCH();
        }
do_add:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            uint8_t reg3 = uZ(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(
                        AS_NUMBER(vm, vmstate_get_reg(vm, reg2))
                        + AS_NUMBER(vm, vmstate_get_reg(vm, reg3))));
            DISPATCH();
        }
do_load_literal:
        {
            uint8_t reg = uX(i);
            uint16_t lit = uYZ(i);
            vmstate_set_reg(vm, reg, vmstate_get_lit(vm, lit));
            DISPATCH();
        }
do_goto:
        {
            int32_t offset = iXYZ(i);
            ip += offset;
            DISPATCH();
        }
do_if:
        {
            uint8_t reg = uX(i);
            if (!value_truthy(vmstate_get_reg(vm, reg)))
                ip++;
            DISPATCH();
        }
    }
    #undef DISPATCH
}
#pragma GCC diagnostic pop
