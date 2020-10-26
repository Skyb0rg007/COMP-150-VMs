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
#include <string.h>

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
#include "svmdebug.h"
#include "disasm.h"

/* 'labels as values' is a GCC extension used for implementing 'vmrun'.
 * As a result, pedantic warnings must be disabled for this function.
 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
void vmrun(VMState vm, struct VMFunction *fun) {

    const char *dump_decode = svmdebug_value("decode");
    const char *dump_call   = svmdebug_value("call");
    (void)dump_call;

    #define push_act(dest) do {                                           \
        struct Activation *__a = &vm->activations[vm->num_activations++]; \
        if (vm->num_activations > NUM_ACTIVATIONS)                        \
           runerror(vm, "Stack overflow!");                               \
        __a->ip = ip;                                                     \
        __a->destreg = (dest);                                            \
        __a->window = vm->window;                                         \
    } while (0)
    #define pop_act(dest) do {                                            \
        struct Activation *__a = &vm->activations[--vm->num_activations]; \
        if (vm->num_activations < 0)                                      \
            runerror(vm, "Stack underflow!");                             \
        ip = __a->ip;                                                     \
        vm->window = __a->window;                                         \
        *(dest) = __a->destreg;                                           \
    } while (0)

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
        [If] = &&do_if,
        [GetGlobal] = &&do_get_global,
        [SetGlobal] = &&do_set_global,
        [Divide] = &&do_divide,
        [Subtract] = &&do_subtract,
        [Multiply] = &&do_multiply,
        [Abs] = &&do_abs,
        [Hash] = &&do_hash,
        [CopyReg] = &&do_copyreg,
        [Call] = &&do_call,
        [Return] = &&do_return,
        [Tailcall] = &&do_tailcall,
        [Error] = &&do_error,
        [TestEq] = &&do_test_eq,
    };
    /* Increments the instruction pointer, jumping to the label in the jump table */
    #define DISPATCH() do {            \
        i = *ip++;                     \
        op = opcode(i);                \
        assert(op >= 0 && op < Unimp); \
        goto *dispatch_table[op];      \
    } while (0)
        /* if (dump_decode) \ */
            /* idump(stderr, vm, pc, instr, window_pos, pRx, pRy, pRz); \ */

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
do_get_global:
        {
            uint8_t reg = uX(i);
            uint8_t lit = uYZ(i);
            Value g = VTable_get(vm->globals, vmstate_get_lit(vm, lit));
            vmstate_set_reg(vm, reg, g);
            DISPATCH();
        }
do_set_global:
        {
            uint8_t reg = uX(i);
            uint8_t lit = uYZ(i);
            VTable_put(vm->globals,
                    vmstate_get_lit(vm, lit),
                    vmstate_get_reg(vm, reg));
            DISPATCH();
        }
do_divide:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            uint8_t reg3 = uZ(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(
                        AS_NUMBER(vm, vmstate_get_reg(vm, reg2))
                        / AS_NUMBER(vm, vmstate_get_reg(vm, reg3))));
            DISPATCH();
        }
do_subtract:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            uint8_t reg3 = uZ(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(
                        AS_NUMBER(vm, vmstate_get_reg(vm, reg2))
                        - AS_NUMBER(vm, vmstate_get_reg(vm, reg3))));
            DISPATCH();
        }
do_multiply:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            uint8_t reg3 = uZ(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(
                        AS_NUMBER(vm, vmstate_get_reg(vm, reg2))
                        * AS_NUMBER(vm, vmstate_get_reg(vm, reg3))));
            DISPATCH();
        }
do_abs:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(
                        fabs(AS_NUMBER(vm, vmstate_get_reg(vm, reg2)))));
            DISPATCH();
        }
do_hash:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(hashvalue(vmstate_get_reg(vm, reg2))));
            DISPATCH();
        }
do_copyreg:
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    vmstate_get_reg(vm, reg2));
            DISPATCH();
        }
do_call:
        {
            int destreg = uX(i);
            int funreg = uY(i);
            int lastarg = uZ(i);
            struct VMFunction *fun = AS_VMFUNCTION(vm, vmstate_get_reg(vm, funreg));
            assert(lastarg - funreg == fun->arity);
            push_act(destreg);
            vm->window += funreg;
            ip = fun->instructions;
            DISPATCH();
        }
do_return:
        {
            int retreg = uX(i);
            Value retval = vmstate_get_reg(vm, retreg);
            int destreg;
            pop_act(&destreg);
            vmstate_set_reg(vm, destreg, retval);
            DISPATCH();
        }
do_tailcall:
        {
            int funreg = uX(i);
            int lastarg = uY(i);
            int num_args = lastarg - funreg;
            Value funVal = vmstate_get_reg(vm, funreg);
            struct VMFunction *fun = AS_VMFUNCTION(vm, funVal);
            vmstate_set_reg(vm, 0, funVal);
            memmove(vm->registers + vm->window,
                    vm->registers + vm->window + funreg,
                    (num_args + 1) * sizeof(Value));
            ip = fun->instructions;
            DISPATCH();
        }
do_error:
        {
            /* XXX */
            abort();
        }
do_test_eq:
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            int reg3 = uZ(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkBooleanValue(AS_NUMBER(vm, vmstate_get_reg(vm, reg2)) == AS_NUMBER(vm, vmstate_get_reg(vm, reg3))));
            DISPATCH();
        }
    }
    #undef DISPATCH
}
#pragma GCC diagnostic pop
