
#include <svm/config.h>
#include <svm/vm.h>
#include <svm/heap.h>
#include <svm/run.h>

/* Note that almost nothing is cached atm
 * This is to hopefully prevent bugs in the implementation, and caching can be added later.
 */
void svm_run(struct svm_vm_t *vm, struct svm_function_t *fun)
{
    /* Push the current activation */
    {
        struct svm_activation_t *current =
            svm_heap_alloc(vm, svm_activation_allocsize(fun->nregs));
        current->forwarded = NULL;
        current->parent = vm->current;
        current->fun = fun;
        current->instr = 0;
        current->nregs = fun->nregs;
        for (int i = 0; i < current->nregs; i++)
            svm_value_set_void(&current->regs[i]);
        vm->current = current;
    }

    for (;;) {
        svm_assert(vm->current->instr < vm->current->fun->size);

        svm_instruction_t instr = vm->current->fun->instructions[vm->current->instr];
        vm->current->instr++;

#if SVM_DEBUG_LEVEL >= 2
        fputs(" \033[38;5;214m", stderr);
        svm_instruction_print(instr, stderr);
        fputs("\033[m\n", stderr);
#endif

        switch (svm_instruction_opcode(instr))
        {
            #define X(_lower, _Title, UPPER, _instrFmt, _formatStr, _desc, code) \
                case SVM_OPCODE_##UPPER:                                         \
                    code                                                         \
                    break;
            #include <svm/opcode-data.h>
            #undef X
            default:
                svm_panic("Invalid instruction!");
        }
    }
}

