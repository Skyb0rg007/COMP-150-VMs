
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
            svm_gc_alloc(vm, svm_activation_allocsize(fun->nregs));
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

        svm_instruction_t i = vm->current->fun->instructions[vm->current->instr];
        vm->current->instr++;
        #define regx svm_instruction_x(i)
        #define regy svm_instruction_y(i)
        #define regz svm_instruction_z(i)
        #define lit  svm_instruction_yz(i)
        #define i24  svm_instruction_xyz(i)

        svm_instruction_print(i, stderr);
        fputs("\n", stderr);

        switch (svm_instruction_opcode(i))
        {
            case SVM_OPCODE_HALT: {
                return;
            }
            case SVM_OPCODE_PRINT: {
                svm_value_print(&vm->current->regs[regx], stdout);
                puts("");
                fflush(stdout);
                break;
            }
            case SVM_OPCODE_ADD: {
                int destreg = regx;
                int arg1reg = regy;
                int arg2reg = regz;

                svm_value_set_number(
                        &vm->current->regs[destreg],
                        svm_value_get_number(&vm->current->regs[arg1reg])
                        +
                        svm_value_get_number(&vm->current->regs[arg2reg]));
                break;
            }
            case SVM_OPCODE_DIVIDE: {
                int destreg = regx;
                int arg1reg = regy;
                int arg2reg = regz;

                svm_value_set_number(
                        &vm->current->regs[destreg],
                        svm_value_get_number(&vm->current->regs[arg1reg])
                        /
                        svm_value_get_number(&vm->current->regs[arg2reg]));
                break;
            }
            case SVM_OPCODE_SUBTRACT: {
                int destreg = regx;
                int arg1reg = regy;
                int arg2reg = regz;

                svm_value_set_number(
                        &vm->current->regs[destreg],
                        svm_value_get_number(&vm->current->regs[arg1reg])
                        -
                        svm_value_get_number(&vm->current->regs[arg2reg]));
                break;
            }
            case SVM_OPCODE_MULTIPLY: {
                int destreg = regx;
                int arg1reg = regy;
                int arg2reg = regz;

                svm_value_set_number(
                        &vm->current->regs[destreg],
                        svm_value_get_number(&vm->current->regs[arg1reg])
                        *
                        svm_value_get_number(&vm->current->regs[arg2reg]));
                break;
            }
            case SVM_OPCODE_ABS: {
                int destreg = regx;
                int argreg = regy;

                svm_value_set_number(
                        &vm->current->regs[destreg],
                        fabs(svm_value_get_number(&vm->current->regs[argreg])));
                break;
            }
            case SVM_OPCODE_COPYREG: {
                int destreg = regx;
                int srcreg = regy;

                vm->current->regs[destreg] = vm->current->regs[srcreg];
                break;
            }
            case SVM_OPCODE_CALL: {
                int destreg = regx;
                int funreg = regy;
                int lastargreg = regz;

                struct svm_value_t *f = &vm->current->regs[funreg];

                switch (f->tag) {
                    case SVM_VALUE_TAG_FUNCTION: {
                        struct svm_function_t *fun = f->rep.as_function;
                        svm_assert_release(lastargreg - funreg == fun->arity);

                        struct svm_activation_t *act =
                            svm_gc_alloc(vm, svm_activation_allocsize(fun->nregs));
                        act->forwarded = NULL;
                        act->parent = vm->current;
                        act->fun = fun;
                        act->instr = 0;
                        act->destreg = destreg;
                        act->nregs = fun->nregs;
                        int i = 0;
                        for (i = 0; i <= fun->arity; i++)
                            act->regs[i] = vm->current->regs[funreg + i];
                        for (; i < act->nregs; i++)
                            svm_value_set_void(&act->regs[i]);

                        vm->current = act;
                        break;
                    }
                    case SVM_VALUE_TAG_CONTINUATION: {
                        struct svm_activation_t *act = f->rep.as_continuation;
                        svm_assert_release(lastargreg - funreg == 1);
                        act->parent->regs[act->destreg] = vm->current->regs[lastargreg];
                        vm->current = act->parent;
                        break;
                    }
                    case SVM_VALUE_TAG_CLOSURE: {
                        svm_panic("Closures nyi");
                    }
                    default:
                        svm_panic("Invalid application of non-function");
                }
                break;
            }
            case SVM_OPCODE_RETURN: {
                int argreg = regx;

                struct svm_activation_t *parent = vm->current->parent;
                svm_assert_release(parent);
                parent->regs[vm->current->destreg] = vm->current->regs[argreg];

                vm->current = parent; /* "pop" */
                break;
            }
            case SVM_OPCODE_LOADLITERAL: {
                vm->current->regs[regx] = svm_vm_loadliteral(vm, lit);
                break;
            }
            case SVM_OPCODE_GOTO: {
                vm->current->instr += i24;
                break;
            }
            case SVM_OPCODE_IF: {
                int condreg = regx;
                if (!svm_value_truthy(&vm->current->regs[condreg]))
                    vm->current->instr++;
                break;
            }
            case SVM_OPCODE_SETGLOBAL:
            case SVM_OPCODE_GETGLOBAL: {
                svm_panic("globals nyi");
            }
            case SVM_OPCODE_CONS: {
                int destreg = regx;
                int carreg = regy;
                int cdrreg = regz;

                struct svm_block_t *pair =
                    svm_gc_alloc(vm, sizeof *pair + 2 * sizeof(struct svm_value_t));
                pair->forwarded = NULL;
                pair->nslots = 2;
                pair->slots[0] = vm->current->regs[carreg];
                pair->slots[1] = vm->current->regs[cdrreg];
                svm_value_set_cons(&vm->current->regs[destreg], pair);
                break;
            }
            case SVM_OPCODE_CAR: {
                int destreg = regx;
                int pairreg = regy;
                vm->current->regs[destreg] =
                    svm_value_get_cons(&vm->current->regs[pairreg])->slots[0];
                break;
            }
            case SVM_OPCODE_CDR: {
                int destreg = regx;
                int pairreg = regy;
                vm->current->regs[destreg] =
                    svm_value_get_cons(&vm->current->regs[pairreg])->slots[1];
                break;
            }
            case SVM_OPCODE_CALLCC: {
                int destreg = regx;
                int funreg = regy;

                struct svm_function_t *fun =
                    svm_value_get_function(&vm->current->regs[funreg]);
                svm_assert_release(fun->arity == 1);

                struct svm_activation_t *act =
                    svm_gc_alloc(vm, svm_activation_allocsize(fun->nregs));
                act->forwarded = NULL;
                act->parent = vm->current;
                act->fun = fun;
                act->instr = 0;
                act->destreg = destreg;
                act->nregs = fun->nregs;
                svm_value_set_function(&act->regs[0], fun);
                svm_value_set_continuation(&act->regs[1], act);
                for (int i = 2; i < fun->nregs; i++)
                    svm_value_set_void(&act->regs[i]);

                vm->current = act;
                break;
            }
            default:
                svm_panic("Opcode %d nyi", svm_instruction_opcode(i));
        }
    }
}

