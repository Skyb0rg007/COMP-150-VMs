/* #define X(lowercase, TitleCase, UPPERCASE, insructionFormat, formatString, descriptionString, codeImpl...) */
/* For the code section, the following variables are available:
 *  svm_instruction_t instr;
 *  struct svm_vm_t *vm;
 */
/* XXX: Remove TitleCase and descriptionString? */

X(halt, Halt, HALT, R0, "halt", "halts code execution", {
    return;
})
X(print, Print, PRINT, R1, "print r%d", "print <reg>", {
    uint8_t reg = svm_instruction_x(instr);

    svm_value_print(&vm->current->regs[reg], stdout);
    fflush(stdout);
})
X(println, Println, PRINTLN, R1, "println r%d", "println <reg>", {
    uint8_t reg = svm_instruction_x(instr);

    svm_value_print(&vm->current->regs[reg], stdout);
    puts("");
    fflush(stdout);
})
X(copyreg, CopyReg, COPYREG, R2, "r%d := r%d", "Copies the value from rY to rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t argreg = svm_instruction_y(instr);

    vm->current->regs[destreg] = vm->current->regs[argreg];
})
X(call, Call, CALL, R3, "r%1$d := r%2$d(r%2$d+1, ..., r%3$d)", "Calls the function rY on args rY+1 ... rZ, placing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t funreg = svm_instruction_y(instr);
    uint8_t lastargreg = svm_instruction_z(instr);

    if (vm->current->regs[funreg].tag == SVM_VALUE_TAG_CONTINUATION) {
        /* XXX: This doesn't work! Need to setup instruction offset as well */
        struct svm_activation_t *act = vm->current->regs[funreg].rep.as_continuation;
        svm_assert_release(lastargreg - funreg == 1);
        act->parent->regs[act->destreg] = vm->current->regs[lastargreg];
        vm->current = act->parent;
    } else {
        struct svm_function_t *fun; 
        switch (vm->current->regs[funreg].tag)
        {
            case SVM_VALUE_TAG_FUNCTION:
                fun = vm->current->regs[funreg].rep.as_function;
                break;
            case SVM_VALUE_TAG_CLOSURE:
                fun = vm->current->regs[funreg].rep.as_closure->fun;
                break;
            default:
                svm_panic("Application of non-procedure %s", svm_value_tag_name(vm->current->regs[funreg].tag));
        }
        svm_assert_release(lastargreg - funreg == fun->arity);
        struct svm_activation_t *act = svm_heap_alloc(vm, svm_activation_allocsize(fun->nregs));
        act->forwarded = NULL;
        act->parent = vm->current;
        act->fun = fun;
        act->instr = 0;
        act->destreg = destreg;
        act->nregs = fun->nregs;
        for (int i = 0; i < act->nregs; i++)
            if (i <= fun->arity)
                act->regs[i] = vm->current->regs[funreg + i];
            else
                svm_value_set_void(&act->regs[i]);
        vm->current = act;
    }
})
X(tailcall, Tailcall, TAILCALL, R2, "tailcall r%1$d(r%1$d+1, ..., r%2$d)", "Tailcalls the function rX on args rX+1 ... rY", {
    uint8_t funreg = svm_instruction_x(instr);
    uint8_t lastargreg = svm_instruction_y(instr);

    if (vm->current->regs[funreg].tag == SVM_VALUE_TAG_CONTINUATION) {
        /* XXX: This doesn't work! Need to setup instruction offset as well */
        struct svm_activation_t *act = vm->current->regs[funreg].rep.as_continuation;
        svm_assert_release(lastargreg - funreg == 1);
        act->parent->regs[act->destreg] = vm->current->regs[lastargreg];
        vm->current = act->parent;
    } else {
        struct svm_function_t *fun; 
        switch (vm->current->regs[funreg].tag)
        {
            case SVM_VALUE_TAG_FUNCTION:
                fun = vm->current->regs[funreg].rep.as_function;
                break;
            case SVM_VALUE_TAG_CLOSURE:
                fun = vm->current->regs[funreg].rep.as_closure->fun;
                break;
            default:
                svm_panic("Application of non-procedure %s", svm_value_tag_name(vm->current->regs[funreg].tag));
        }
        svm_assert_release(lastargreg - funreg == fun->arity);
        struct svm_activation_t *act = svm_heap_alloc(vm, svm_activation_allocsize(fun->nregs));
        act->forwarded = NULL;
        act->parent = vm->current->parent;
        act->fun = fun;
        act->instr = 0;
        act->destreg = vm->current->destreg;
        act->nregs = fun->nregs;
        for (int i = 0; i < act->nregs; i++)
            if (i <= fun->arity)
                act->regs[i] = vm->current->regs[funreg + 1];
            else
                svm_value_set_void(&act->regs[i]);
        vm->current = act;
    }

})
X(hash, Hash, HASH, R2, "r%d := hash(r%d)", "Hashes rY, placing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t argreg = svm_instruction_y(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            svm_value_hash(&vm->current->regs[argreg]));
})
X(return, Return, RETURN, R1, "return r%d", "Returns from the function with result rX", {
    uint8_t argreg = svm_instruction_x(instr);

    struct svm_activation_t *parent = vm->current->parent;
    svm_assert_release(parent != NULL);
    parent->regs[vm->current->destreg] = vm->current->regs[argreg];
    vm->current = parent;
})
X(loadliteral, LoadLiteral, LOADLITERAL, R1LIT, "r%d := _LIT[%d]", "loadliteral <destreg> <literal>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint16_t litindex = svm_instruction_yz(instr);

    vm->current->regs[destreg] = svm_vm_loadliteral(vm, litindex);
})
X(goto, Goto, GOTO, R0I24, "goto %d", "Adjusts the instruction pointer by rXYZ", {
    int32_t offset = svm_instruction_xyz(instr);

    vm->current->instr += offset;
})
X(if, If, IF, R1, "if r%d", "Increments instruction pointer if rX is #f", {
    uint8_t condreg = svm_instruction_x(instr);

    if (!svm_value_truthy(&vm->current->regs[condreg])) {
        vm->current->instr++;
    }
})

/* Math ops */
X(add, Add, ADD, R3, "r%d := r%d + r%d", "adds rY and rZ, storing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t arg1reg = svm_instruction_y(instr);
    uint8_t arg2reg = svm_instruction_z(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            svm_value_get_number(&vm->current->regs[arg1reg])
            +
            svm_value_get_number(&vm->current->regs[arg2reg]));
})
X(div, Div, DIV, R3, "r%d := r%d / r%d", "divides rY by rZ, storing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t arg1reg = svm_instruction_y(instr);
    uint8_t arg2reg = svm_instruction_z(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            svm_value_get_number(&vm->current->regs[arg1reg])
            /
            svm_value_get_number(&vm->current->regs[arg2reg]));
})
X(idiv, IDiv, IDIV, R3, "r%d := r%d // r%d", "Integer-divides rY by rZ, storing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t arg1reg = svm_instruction_y(instr);
    uint8_t arg2reg = svm_instruction_z(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            (uint64_t)roundeven(svm_value_get_number(&vm->current->regs[arg1reg]))
            /
            (uint64_t)roundeven(svm_value_get_number(&vm->current->regs[arg2reg])));
})
X(sub, Sub, SUB, R3, "r%d := r%d - r%d", "subtracts rY and rZ, storing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t arg1reg = svm_instruction_y(instr);
    uint8_t arg2reg = svm_instruction_z(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            svm_value_get_number(&vm->current->regs[arg1reg])
            -
            svm_value_get_number(&vm->current->regs[arg2reg]));
})
X(mul, Mul, MUL, R3, "r%d := r%d * r%d", "multiplies rY and rZ, storing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t arg1reg = svm_instruction_y(instr);
    uint8_t arg2reg = svm_instruction_z(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            svm_value_get_number(&vm->current->regs[arg1reg])
            *
            svm_value_get_number(&vm->current->regs[arg2reg]));
})
X(abs, Abs, ABS, R2, "r%d := abs(r%d)", "Takes the absolute value of rY, storing result in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t argreg = svm_instruction_y(instr);

    svm_value_set_number(
            &vm->current->regs[destreg],
            fabs(svm_value_get_number(&vm->current->regs[argreg])));
})

/* Global variables */
X(setglobal, SetGlobal, SETGLOBAL, R1LIT, "_G[_LIT[%2$d]] := r%1$d", "Sets the global LIT to value rX", {
    uint8_t argreg = svm_instruction_x(instr);
    uint16_t litindex = svm_instruction_yz(instr);

    struct svm_value_t key = svm_vm_loadliteral(vm, litindex);
    struct svm_value_t val = vm->current->regs[argreg];
    uint32_t it = svm_hash_put(&vm->globals, key, NULL, &vm->allocator);
    svm_hash_val(&vm->globals, it) = val;
})
X(getglobal, GetGlobal, GETGLOBAL, R1LIT, "r%d := _G[_LIT[%d]]", "Gets the global LIT and stores it in rX", {
    uint8_t destreg = svm_instruction_x(instr);
    uint16_t litindex = svm_instruction_yz(instr);

    struct svm_value_t key = svm_vm_loadliteral(vm, litindex);
    uint32_t it = svm_hash_get(&vm->globals, key);
    if (it == svm_hash_end(&vm->globals))
        svm_value_set_void(&vm->current->regs[destreg]);
    else
        vm->current->regs[destreg] = svm_hash_val(&vm->globals, it);
})

/* Testing */
X(check, Check, CHECK, R1LIT, "check %d %d", "First part of check-expect", {
    uint8_t argreg = svm_instruction_x(instr);
    uint16_t litindex = svm_instruction_yz(instr);
    svm_assert(vm->checks.source == NULL);
    svm_assert(vm->checks.value == NULL);
    struct svm_value_t valstr = svm_vector_at(&vm->literals, litindex);
    vm->checks.source = strdup(svm_value_get_string(&valstr)->bytes);
    vm->checks.value = &vm->current->regs[argreg];
})
X(expect, Expect, EXPECT, R1LIT, "expect %d %d", "Second part of check-expect", {
    uint8_t argreg = svm_instruction_x(instr);
    uint16_t litindex = svm_instruction_yz(instr);

    svm_assert(vm->checks.source != NULL);
    svm_assert(vm->checks.value != NULL);
    struct svm_value_t valstr = svm_vm_loadliteral(vm, litindex);
    char *source = svm_value_get_string(&valstr)->bytes;
    struct svm_value_t *value = &vm->current->regs[argreg];

    vm->checks.ntests++;
    if (svm_value_equal(vm->checks.value, value)) {
        vm->checks.npassed++;
    } else {
        fprintf(stderr, "(check-expect %s %s) failed, got ", vm->checks.source, source);
        svm_value_print(value, stderr);
        fprintf(stderr, ", expected ");
        svm_value_print(vm->checks.value, stderr);
        fprintf(stderr, ".\n");
    }
    free(vm->checks.source);
    vm->checks.source = NULL;
    vm->checks.value = NULL;
})
X(checkassert, CheckAssert, CHECKASSERT, R2, "check-assert %d %d", "check-assert", {
    uint8_t argreg = svm_instruction_x(instr);
    uint16_t litindex = svm_instruction_yz(instr);

    struct svm_value_t valstr = svm_vm_loadliteral(vm, litindex);
    char *source = svm_value_get_string(&valstr)->bytes;
    struct svm_value_t *value = &vm->current->regs[argreg];

    vm->checks.ntests++;
    if (svm_value_truthy(value)) {
        vm->checks.npassed++;
    } else {
        fprintf(stderr, "(check-assert %s) failed, got ", source);
        svm_value_print(value, stderr);
        fprintf(stderr, ".\n");
    }
})

/******************************************************************************
 * Closures
 *****************************************************************************/
X(closure, Closure, CLOSURE, R3, "r%d := closure r%d %d", "closure <destreg> <funreg> <nslots>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t funreg = svm_instruction_y(instr);
    uint8_t nslots = svm_instruction_z(instr);

    struct svm_closure_t *closure = svm_heap_alloc(vm, svm_closure_allocsize(nslots));
    closure->forwarded = NULL;
    closure->fun = svm_value_get_function(&vm->current->regs[funreg]);
    closure->nslots = nslots;
    for (int i = 0; i < nslots; i++)
        svm_value_set_void(&closure->slots[i]);
    svm_value_set_closure(&vm->current->regs[destreg], closure);
})
X(getclslot, GetClSlot, GETCLSLOT, R3, "r%d := r%d.%d", "getclslot <destreg> <clreg> <slotnum>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t clreg   = svm_instruction_y(instr);
    uint8_t slotnum = svm_instruction_z(instr);

    struct svm_closure_t *closure = svm_value_get_closure(&vm->current->regs[clreg]);
    svm_assert(slotnum < closure->nslots);
    vm->current->regs[destreg] = closure->slots[slotnum];
})
X(setclslot, SetClSlot, SETCLSLOT, R3, "r%1$d.%3$d := r%2$d", "setclslot <clreg> <valreg> <slotnum>", {
    uint8_t clreg   = svm_instruction_x(instr);
    uint8_t valreg  = svm_instruction_y(instr);
    uint8_t slotnum = svm_instruction_z(instr);

    struct svm_closure_t *closure = svm_value_get_closure(&vm->current->regs[clreg]);
    svm_assert(slotnum < closure->nslots);
    closure->slots[slotnum] = vm->current->regs[valreg];
})

/******************************************************************************
 * Boxes
 *****************************************************************************/
X(box, Box, BOX, R2, "r%d := box r%d", "box <destreg> <valreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t valreg = svm_instruction_y(instr);

    struct svm_box_t *box = svm_heap_alloc(vm, sizeof *box);
    box->forwarded = NULL;
    box->val = vm->current->regs[valreg];
    svm_value_set_box(&vm->current->regs[destreg], box);
})
X(boxq, BoxQ, BOXQ, R2, "r%d := boxq r%d", "boxq <destreg> <valreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t valreg = svm_instruction_y(instr);

    svm_value_set_boolean(
            &vm->current->regs[destreg],
            vm->current->regs[valreg].tag == SVM_VALUE_TAG_BOX);
})
X(unbox, Unbox, UNBOX, R2, "r%d := unbox r%d", "unbox <destreg> <boxreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t boxreg = svm_instruction_y(instr);

    struct svm_box_t *box = svm_value_get_box(&vm->current->regs[boxreg]);
    vm->current->regs[destreg] = box->val;
})
X(set_box, Set_Box, SET_BOX, R2, "set_box r%d r%d", "set_box <boxreg> <valreg>", {
    uint8_t boxreg = svm_instruction_x(instr);
    uint8_t valreg = svm_instruction_y(instr);

    struct svm_box_t *box = svm_value_get_box(&vm->current->regs[boxreg]);
    box->val = vm->current->regs[valreg];
})

/******************************************************************************
 * Void
 *****************************************************************************/
X(void, Void, VOID, R1, "void r%d", "void <destreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    svm_value_set_void(&vm->current->regs[destreg]);
})

/******************************************************************************
 * Pairs
 *****************************************************************************/
X(cons, Cons, CONS, R3, "r%d := cons r%d r%d", "cons <destreg> <carreg> <cdrreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t carreg  = svm_instruction_y(instr);
    uint8_t cdrreg  = svm_instruction_z(instr);

    struct svm_cons_t *cons = svm_heap_alloc(vm, sizeof *cons);
    cons->forwarded = NULL;
    cons->car = vm->current->regs[carreg];
    cons->cdr = vm->current->regs[cdrreg];
    svm_value_set_cons(&vm->current->regs[destreg], cons);
})
X(pairq, PairQ, PAIRQ, R2, "r%d := pairq r%d", "pairq <destreg> <valreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t valreg  = svm_instruction_y(instr);

    svm_value_set_boolean(
            &vm->current->regs[destreg],
            vm->current->regs[valreg].tag == SVM_VALUE_TAG_CONS);
})
X(car, Car, CAR, R2, "r%d := car r%d", "car <destreg> <pairreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t consreg = svm_instruction_y(instr);

    struct svm_cons_t *cons = svm_value_get_cons(&vm->current->regs[consreg]);
    vm->current->regs[destreg] = cons->car;
})
X(cdr, Cdr, CDR, R2, "r%d := cdr r%d", "cdr <destreg> <pairreg>", {
    uint8_t destreg = svm_instruction_x(instr);
    uint8_t consreg = svm_instruction_y(instr);

    struct svm_cons_t *cons = svm_value_get_cons(&vm->current->regs[consreg]);
    vm->current->regs[destreg] = cons->cdr;
})
X(set_car, Set_Car, SET_CAR, R2, "r%d.car := r%d", "set_car <pairreg> <valreg>", {
    uint8_t consreg = svm_instruction_x(instr);
    uint8_t valreg  = svm_instruction_y(instr);

    struct svm_cons_t *cons = svm_value_get_cons(&vm->current->regs[consreg]);
    cons->car = vm->current->regs[valreg];
})
X(set_cdr, Set_Cdr, SET_CDR, R2, "r%d.cdr := r%d", "set_cdr <pairreg> <valreg>", {
    uint8_t consreg = svm_instruction_x(instr);
    uint8_t valreg  = svm_instruction_y(instr);

    struct svm_cons_t *cons = svm_value_get_cons(&vm->current->regs[consreg]);
    cons->cdr = vm->current->regs[valreg];
})
