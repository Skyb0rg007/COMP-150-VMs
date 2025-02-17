// The VM loader: read virtual object code and update VM state

// This is the focus of module 2.  In future modules, we'll continue
// to use it, but we won't revise it except for an optional depth goal.
//
// You'll write the core of the loader: function `loadfun`.  This
// function loads a single VM function from a .vo file.  It is called
// by `loadmodule`, which load a single module, and which I provide.
// (A module is just a function that takes no parameters).  I also
// provide `loadmodules`, which loads a list of modules.  Finally, I
// provide `parse_instruction`, which uses the parsing table in file
// instructions.c to parse each single instruction.



#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "loader.h"
#include "itable.h"
#include "tokens.h"
#include "svmdebug.h"
#include "vmstate.h"
#include "vmheap.h"
#include "vmstring.h"
#include "value.h"

//// Variables and utility functions local to this module

static bool saw_a_bad_opcode = false;  

static Instruction
  parse_instruction(VMState vm, Name opcode, Tokens operands, unsigned *maxregp);
  // Parse `operands` according to the parser for the given `opcode`.
  // Return the parsed instruction.  If the instruction mentions any
  // register, update `*maxregp` to be at least as large as any
  // mentioned register (and no smaller than it was).
  //
  // If the opcode is not recognized, set `saw_a_bad_opcode` and return 0.

static Modules mkModules(struct VMFunction *first, Modules rest);
  // using malloc, make a cons cell for a linked list of modules


//// The function you will write

static struct VMFunction *loadfun(VMState vm, int arity, int count, FILE *input);
  // Allocates and returns a new function using `vmalloc`, with all fields
  // correctly set:
  //   - The arity is given by parameter `arity`.
  //   - The number of instructions is given by parameter `count`.
  //   - The instructions themselves are read from `input`, one per line.
  //   - The `nregs` field must be set to one more than the largest
  //     register mentioned in any instruction.


static struct VMFunction *loadfun(VMState vm, int arity, int count, FILE *input) {

    struct VMFunction *fun = vmalloc(sizeof *fun + (count + 1) * sizeof(Instruction));
    assert(fun);
    unsigned maxreg = 0;
    for (int i = 0; i < count; i++) {
        char *buffer = NULL;
        size_t bufsize = 0;
        if (getline(&buffer, &bufsize, input) < 0) {
            assert(0);
        }
        Tokens operands = tokens(buffer);
        Name opcode = tokens_get_name(&operands, buffer);
        Instruction instr;
        if (opcode == strtoname(".load")) {
            uint8_t reg = tokens_get_byte(&operands, buffer);
            Name f = tokens_get_name(&operands, buffer);
            assert(f == strtoname("function"));
            uint32_t arity = tokens_get_int(&operands, buffer);
            uint32_t len = tokens_get_int(&operands, buffer);
            struct VMFunction *func = loadfun(vm, arity, len, input);
            int func_lit = literal_slot(vm, mkVMFunctionValue(func));
            instr = eR1U16(LoadLiteral, reg, func_lit);
        } else {
            instr = parse_instruction(vm, opcode, operands, &maxreg);
        }
        fun->instructions[i] = instr;
    }
    fun->instructions[count] = Halt; /* TODO: Encode this */

    fun->arity = arity;
    fun->size = count + 1;
    fun->nregs = maxreg + 1;
    return fun;
}


//// module loading, with helper function

static bool has_input(FILE *fd) {
  int c = getc(fd);
  if (c == EOF) {
    return false;
  } else {
    ungetc(c, fd);
    return true;
  }
}


static struct VMFunction *loadmodule(VMState vm, FILE *vofile) {
  // precondition: `vofile` has input remaining

  static Name dotloadname, modulename;
  if (dotloadname == NULL) {
    dotloadname = strtoname(".load");
    modulename  = strtoname("module");
  }

  // read a line from `vofile` and tokenize it
  char *buffer = NULL;
  size_t bufsize;
  if (getline(&buffer, &bufsize, vofile) < 0) {
    // end of file, spec calls for NULL to be returned
    assert(false);
  }
  Tokens alltokens = tokens(buffer);
  Tokens tokens_left = alltokens;

  // parse the tokens; expecting ".load module <count>"
  Name n;
  n = tokens_get_name(&tokens_left, buffer); // removes token from tokens_left
  assert(n == dotloadname);
  n = tokens_get_name(&tokens_left, buffer);
  assert(n == modulename);
  uint32_t count = tokens_get_int(&tokens_left, buffer);
  assert(tokens_left == NULL); // that must be the last token

  free(buffer); // done with the input line and its tokens
  free_tokens(&alltokens);

  return loadfun(vm, 0, count, vofile); // read the remaining instructions
}


Modules loadmodules(VMState vm, FILE *vofile) {
  if (has_input(vofile)) {
    struct VMFunction *module = loadmodule(vm, vofile); // load the first
    return mkModules(module, loadmodules(vm, vofile)); // and iterate
  } else {
    return NULL;
  }
}
    

///// utility functions

static Modules mkModules(struct VMFunction *first, Modules rest) {
  Modules ms = malloc(sizeof(*ms));
  assert(ms);
  ms->module = first;
  ms->next   = rest;
  return ms;
}

void freemodules(Modules *msp) {
  if (*msp) {
    freemodules(&(*msp)->next);
    free(*msp);
    *msp = NULL;
  }
}

static Instruction
parse_instruction(VMState vm, Name opcode, Tokens operands, unsigned *maxregp) {
  instruction_info *info = itable_entry(opcode);
  if (info) {
    return info->parser(vm, info->opcode, operands, maxregp);
  } else {
    fprintf(stderr, "No opcode for %s.\n", nametostr(opcode));
    saw_a_bad_opcode = true;
    return 0;
  }
}
