// Defines all the opcodes used in the VM

// When you're thinking about new instructions, define them here first.
// It's OK for an opcode to be defined here even if it is not implemented 
// anywhere.  But if you want to *run* an instruction (module 1) or *load*
// an instruction (module 2), the opcode has to be defined here first.

#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

/* Calls the macro on each opcode, with the given separator
 * First argument is lowercase, then TitleCase, then UPPERCASE
 */
#define FOREACH_OPCODE(_)                    \
    _(halt, Halt, HALT)                      \
    _(print, Print, PRINT)                   \
    _(check, Check, CHECK)                   \
    _(expect, Expect, EXPECT)                \
    _(add, Add, ADD)                         \
    _(loadliteral, LoadLiteral, LOADLITERAL) \
    _(goto, Goto, GOTO)                      \
    _(if, If, IF)                            \
    _(getglobal, GetGlobal, GETGLOBAL)       \
    _(setglobal, SetGlobal, SETGLOBAL)       \
    _(divide, Divide, DIVIDE)                \
    _(subtract, Subtract, SUBTRACT)          \
    _(multiply, Multiply, MULTIPLY)          \
    _(abs, Abs, ABS)                         \
    _(hash, Hash, HASH)                      \
    _(copyreg, CopyReg, COPYREG)             \
    _(call, Call, CALL)                      \
    _(return, Return, RETURN)                \
    _(tailcall, Tailcall, TAILCALL)          \
    _(error, Error, ERROR)                   \
    _(testeq, TestEq, TESTEQ)                \
    _(getclslot, GetClSlot, GETCLSLOT)       \
    _(setclslot, SetClSlot, SETCLSLOT)       \
    _(mkclosure, MkClosure, MKCLOSURE)       \
    _(cons, Cons, CONS)                      \
    _(car, Car, CAR)                         \
    _(cdr, Cdr, CDR)                         \
    _(lt, Lt, LT)                            \
    _(gt, Gt, GT)                            \
    _(null_chk, Null_Chk, NULL_CHK)          \
    _(symbol_chk, Symbol_Chk, SYMBOL_CHK)    \
    _(number_chk, Number_Chk, NUMBER_CHK)    \
    _(boolean_chk, Boolean_Chk, BOOLEAN_CHK) \
    _(gc, Gc, GC)

/* Define the enum using the title-case version of the opcode names */
#define X(lower, title, upper) title,
typedef enum {
    FOREACH_OPCODE(X)
    Unimp
} Opcode;
#undef X

#endif
