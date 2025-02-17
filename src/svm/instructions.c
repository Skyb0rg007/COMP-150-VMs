// List of all opcodes, parsers, and unparsers

// You'll develop this list from module 2 onward.  Every time
// you add a new instruction, you'll add an entry here.
// You'll also define the opcode in file opcodes.h,
// and you'll add a case to your `vmrun` function.

#include "iformat.h"
#include "name.h"
#include "itable.h"

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

instruction_info instructions[] = {
  { "halt", Halt, parseR0, "halt" },
  { "print", Print, parseR1, "print rX" },
  { "loadliteral", LoadLiteral, parseR1LIT, "rX := LIT" },
  { "check", Check, parseR1LIT, "check rX, LIT" },
  { "expect", Expect, parseR1LIT, "expect rX, LIT" },
  { "add", Add, parseR3, "rX := rY + rZ" }, { "+", Add, parseR3, "rX := rY + rZ" },
  { "goto", Goto, parseR0I24, "ip += XYZ" },
  { "if", If, parseR1, "if (rX) ip++" },
  { "getglobal", GetGlobal, parseR1LIT, "rX := _G[LIT]" },
  { "setglobal", SetGlobal, parseR1LIT, "_G[LIT] := rY" },
  { "div", Divide, parseR3, "rX := rY / rZ" }, { "/", Divide, parseR3, "rX := rY / rZ" },
  { "sub", Subtract, parseR3, "rX := rY - rZ" }, { "-", Subtract, parseR3, "rX := rY - rZ" },
  { "mul", Multiply, parseR3, "rX := rY * rZ" }, { "*", Multiply, parseR3, "rX := rY * rZ" },
  { "abs", Abs, parseR2, "rX := abs(rY)" },
  { "hash", Hash, parseR2, "rX := hash(rY)" },
  { "copyreg", CopyReg, parseR2, "rX := rY" },
  { "call", Call, parseR3, "rX := call rY (rY + 1, ..., rZ)" },
  { "return", Return, parseR1, "return rX" },
  { "tailcall", Tailcall, parseR2, "tailcall rX (rX + 1, ..., rY)" },
  { "error", Error, parseR1, "error rX" },
  { "=", TestEq, parseR3, "rX := rY = rZ" },
};

int number_of_instructions = sizeof(instructions) / sizeof(instructions[0]);
