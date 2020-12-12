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
void vmrun(VMState vm, struct VMFunction *fun) {

    vm->current_fun = fun;
    /* Points to the current instruction pointer */
    Instruction *ip = vm->current_fun->instructions + vm->current_instrnum;
    /* The current instruction */
    Instruction i;
    /* The current opcode */
    Opcode op;

    #define VMSAVE() do {                                          \
        vm->current_instrnum = vm->current_fun->instructions - ip; \
    } while (0)

    #define VMLOAD() do {                                          \
        ip = vm->current_fun->instructions + vm->current_instrnum; \
    } while (0)

    #define GC() do { \
        VMSAVE();     \
        gc(vm);       \
        VMLOAD();     \
    } while (0)

    /* This code is run before every instruction */
    #define SETUP() do {               \
        i = *ip++;                     \
        op = opcode(i);                \
        assert(op >= 0 && op < Unimp); \
    } while (0)

    /* #define USE_COMPUTED_GOTO */
#ifdef USE_COMPUTED_GOTO
    /* Define the jump-table mapping opcodes to labels
     * Use the TitleCase names to match case versions
     */
    #define AddrLbl(lbl) (__extension__ &&lbl)
    #define X(lower, title, upper) [title] = AddrLbl(do_##title),
    static const void *jmptbl[] = {
        FOREACH_OPCODE(X)
        AddrLbl(do_default),
        AddrLbl(do_default),
        AddrLbl(do_default),
        AddrLbl(do_default)
    };
    #undef X
    #undef AddrLbl

    /* Transform 'case' and 'default' into normal labels */
    #define CASE(x) do_##x
    #define DEFAULT do_default
    /* Transform 'break' into a dispatcher */
    #define BREAK do {                                   \
        SETUP();                                         \
        _Pragma("GCC diagnostic push");                  \
        _Pragma("GCC diagnostic ignored\"-Wpedantic\""); \
        goto *jmptbl[op];                                \
        _Pragma("GCC diagnostic pop")                    \
    } while (0)
    /* Start the loop by dispatching.
     * Nothing has to be done at the end. */
    #define BEGIN_LOOP BREAK;
    #define END_LOOP   (void)0

#else /* ifdef USE_COMPUTED_GOTO */

    #define CASE(x) case x
    #define DEFAULT default
    /* Use a label + goto instead of a 'while' loop.
     * This allows for breaking out of a loop within the interpreter loop */
    #define BREAK goto begin_loop
    #define BEGIN_LOOP  \
        begin_loop: {   \
            SETUP();    \
            switch (op)
    #define END_LOOP }

#endif /* ifdef USE_COMPUTED_GOTO */

    const char *dump_decode = svmdebug_value("decode");
    const char *dump_call   = svmdebug_value("call");
    (void)dump_decode;
    (void)dump_call;

    BEGIN_LOOP {
CASE(Halt):
        return;
CASE(Print):
        {
            uint8_t reg = uX(i);
            print("%v\n", vmstate_get_reg(vm, reg));
            BREAK;
        }
CASE(Check):
        {
            uint8_t reg = uX(i);
            uint16_t lit = uYZ(i);
            check(AS_CSTRING(vm, vmstate_get_lit(vm, lit)), vmstate_get_reg(vm, reg));
            BREAK;
        }
CASE(Expect):
        {
            uint8_t reg = uX(i);
            uint16_t lit = uYZ(i);
            expect(AS_CSTRING(vm, vmstate_get_lit(vm, lit)), vmstate_get_reg(vm, reg));
            BREAK;
        }
CASE(Add):
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
            BREAK;
        }
CASE(LoadLiteral):
        {
            uint8_t reg = uX(i);
            uint16_t lit = uYZ(i);
            vmstate_set_reg(vm, reg, vmstate_get_lit(vm, lit));
            BREAK;
        }
CASE(Goto):
        {
            int32_t offset = iXYZ(i);
            ip += offset;

            if (offset < 0 && gc_needed) {
                GC();
            }
            BREAK;
        }
CASE(If):
        {
            uint8_t reg = uX(i);
            if (!value_truthy(vmstate_get_reg(vm, reg)))
                ip++;
            BREAK;
        }
CASE(GetGlobal):
        {
            uint8_t reg = uX(i);
            uint8_t lit = uYZ(i);
            Value g = VTable_get(vm->globals, vmstate_get_lit(vm, lit));
            vmstate_set_reg(vm, reg, g);
            BREAK;
        }
CASE(SetGlobal):
        {
            uint8_t reg = uX(i);
            uint8_t lit = uYZ(i);
            VTable_put(vm->globals,
                    vmstate_get_lit(vm, lit),
                    vmstate_get_reg(vm, reg));
            BREAK;
        }
CASE(Divide):
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
            BREAK;
        }
CASE(Subtract):
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
            BREAK;
        }
CASE(Multiply):
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
            BREAK;
        }
CASE(Abs):
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(
                        fabs(AS_NUMBER(vm, vmstate_get_reg(vm, reg2)))));
            BREAK;
        }
CASE(Hash):
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkNumberValue(hashvalue(vmstate_get_reg(vm, reg2))));
            BREAK;
        }
CASE(CopyReg):
        {
            uint8_t reg1 = uX(i);
            uint8_t reg2 = uY(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    vmstate_get_reg(vm, reg2));
            BREAK;
        }
CASE(Call):
        {
            int destreg = uX(i);
            int funreg = uY(i);
            int lastarg = uZ(i);
            struct VMFunction *fun = AS_VMFUNCTION(vm, vmstate_get_reg(vm, funreg));
            assert(lastarg - funreg == fun->arity);
            vmstate_store_act(vm, fun, destreg);
            vm->window += funreg;
            ip = fun->instructions;

            if (gc_needed)
                GC();
            BREAK;
        }
CASE(Return):
        {
            int retreg = uX(i);
            Value retval = vmstate_get_reg(vm, retreg);
            int destreg = vmstate_restore_act(vm, &ip);
            vmstate_set_reg(vm, destreg, retval);
            BREAK;
        }
CASE(Tailcall):
        {
            int funreg = uX(i);
            int lastarg = uY(i);
            int num_args = lastarg - funreg;
            Value funVal = vmstate_get_reg(vm, funreg);
            struct VMFunction *fun = GCVALIDATE(AS_VMFUNCTION(vm, funVal));
            /* vmstate_set_reg(vm, 0, funVal); */
            /* XXX: bounds checking before memmove */
            memmove(vm->registers + vm->window,
                    vm->registers + vm->window + funreg,
                    (num_args + 1) * sizeof(Value));
            ip = fun->instructions;

            if (gc_needed)
                GC();
            BREAK;
        }
CASE(Error):
        {
            /* XXX */
            abort();
            BREAK;
        }
CASE(TestEq):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            int reg3 = uZ(i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkBooleanValue(AS_NUMBER(vm, vmstate_get_reg(vm, reg2)) == AS_NUMBER(vm, vmstate_get_reg(vm, reg3))));
            BREAK;
        }
CASE(GetClSlot):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            int reg3 = uZ(i);
            struct VMClosure *closure = GCVALIDATE(AS_CLOSURE(vm, vmstate_get_reg(vm, reg2)));
            int index = AS_NUMBER(vm, vmstate_get_reg(vm, reg3));
            vmstate_set_reg(
                    vm,
                    reg1,
                    closure->captured[index]);
            BREAK;
        }
CASE(SetClSlot):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            int reg3 = uZ(i);
            struct VMClosure *closure = GCVALIDATE(AS_CLOSURE(vm, vmstate_get_reg(vm, reg1)));
            int index = AS_NUMBER(vm, vmstate_get_reg(vm, reg3));
            closure->captured[index] = vmstate_get_reg(vm, reg2);
            BREAK;
        }
CASE(MkClosure):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            int reg3 = uZ(i);
            int ncaptured = reg3 - reg2;
            struct VMFunction *f = GCVALIDATE(AS_VMFUNCTION(vm, vmstate_get_reg(vm, reg2)));
            VMNEW(struct VMClosure *, closure, sizeof *closure + ncaptured);
            closure->f = f;
            for (int i = 0; i < ncaptured; i++)
                closure->captured[i] = vmstate_get_reg(vm, reg2 + i);
            vmstate_set_reg(
                    vm,
                    reg1,
                    mkClosureValue(closure));
            BREAK;
        }
CASE(Cons):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            int reg3 = uZ(i);
            VMNEW(struct VMBlock *, pair, sizeof *pair + sizeof(struct Value) * 2);
            pair->nslots = 2;
            pair->slots[0] = vmstate_get_reg(vm, reg2);
            pair->slots[1] = vmstate_get_reg(vm, reg3);
            vmstate_set_reg(vm, reg1, mkBlockValue(pair));
            BREAK;
        }
CASE(Car):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            struct VMBlock *pair = GCVALIDATE(AS_BLOCK(vm, vmstate_get_reg(vm, reg2)));
            vmstate_set_reg(vm, reg1, pair->slots[0]);
            BREAK;
        }
CASE(Cdr):
        {
            int reg1 = uX(i);
            int reg2 = uY(i);
            struct VMBlock *pair = GCVALIDATE(AS_BLOCK(vm, vmstate_get_reg(vm, reg2)));
            vmstate_set_reg(vm, reg1, pair->slots[1]);
            BREAK;
        }
CASE(Lt):
        {
            BREAK;
        }
CASE(Gt):
        {
            BREAK;
        }
CASE(Null_Chk):
        {
            BREAK;
        }
CASE(Symbol_Chk):
        {
            BREAK;
        }
CASE(Number_Chk):
        {
            BREAK;
        }
CASE(Boolean_Chk):
        {
            BREAK;
        }
CASE(Gc):
        {
            GC();
            BREAK;
        }
DEFAULT:
        {
            abort();
            BREAK;
        }
    }
    END_LOOP;
}
