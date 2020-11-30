#ifndef SVM_OPCODES_H
#define SVM_OPCODES_H

/** Calls the given macro over every opcode.
 * X(lowercase, TitleCase, UPPERCASE, format, example)
 * lowercase, TitleCase, UPPERCASE, format are tokens
 * example is a string
 * format is one of:
 *   R0
 *   R1
 *   R2
 *   R3
 *   R1LIT
 *   R0I24
 */
#define SVM_FOREACH_OPCODE(_)                                                  \
    _(halt,        Halt,        HALT,        R0,    "halt")                    \
    _(print,       Print,       PRINT,       R1,    "print rX")                \
    _(check,       Check,       CHECK,       R1LIT, "check rX LIT")            \
    _(expect,      Expect,      EXPECT,      R1LIT, "expect rX LIT")           \
    _(add,         Add,         ADD,         R3,    "rX := rY + rZ")           \
    _(loadliteral, LoadLiteral, LOADLITERAL, R1LIT, "rX := LIT")               \
    _(goto,        Goto,        GOTO,        R0I24, "ip += XYZ")               \
    _(if,          If,          IF,          R1,    "if (rX) ip++")            \
    _(getglobal,   GetGlobal,   GETGLOBAL,   R1LIT, "rX := globals[LIT]")      \
    _(setglobal,   SetGlobal,   SETGLOBAL,   R1LIT, "globals[LIT] := rX")      \
    _(divide,      Divide,      DIVIDE,      R3,    "rX := rY / rZ")           \
    _(subtract,    Subtract,    SUBTRACT,    R3,    "rX := rY - rZ")           \
    _(multiply,    Multiply,    MULTIPLY,    R3,    "rX := rY * rZ")           \
    _(abs,         Abs,         ABS,         R2,    "rX := abs rY")            \
    _(hash,        Hash,        HASH,        R2,    "rX := hash rY")           \
    _(copyreg,     CopyReg,     COPYREG,     R2,    "rX := rY")                \
    _(call,        Call,        CALL,        R3,    "rX := rY(rY+1, ..., rZ)") \
    _(return,      Return,      RETURN,      R1,    "return rX")               \
    _(tailcall,    Tailcall,    TAILCALL,    R2,    "rX(rX+1, ..., rY)")       \
    _(error,       Error,       ERROR,       R1,    "error rX")                \
    _(getclslot,   GetClSlot,   GETCLSLOT,   R3,    "rX := rY[rZ]")            \
    _(setclslot,   SetClSlot,   SETCLSLOT,   R3,    "rX[rY] := rZ")            \
    _(mkclosure,   MkClosure,   MKCLOSURE,   R3,    "rX := closure[rY, Z]")    \
    _(cons,        Cons,        CONS,        R3,    "rX := cons rY rZ")        \
    _(car,         Car,         CAR,         R2,    "rX := car rY")            \
    _(cdr,         Cdr,         CDR,         R2,    "rX := cdr rY")            \
    _(eq,          Eq,          EQ,          R3,    "rX := rY = rZ")           \
    _(lt,          Lt,          LT,          R3,    "rX := rY < rZ")           \
    _(gt,          Gt,          GT,          R3,    "rX := rY > rZ")           \
    _(nullq,       Nullq,       NULLQ,       R2,    "rX := null? rY")          \
    _(symbolq,     Symbolq,     SYMBOLQ,     R2,    "rX := symbol? rY")        \
    _(numberq,     Numberq,     NUMBERQ,     R2,    "rX := number? rY")        \
    _(booleanq,    Booleanq,    BOOLEANQ,    R2,    "rX := boolean? rY")       \
    _(gc,          Gc,          GC,          R0,    "gc")                      \
    _(callcc,      CallCC,      CALLCC,      R2,    "rX := callcc rY")         \
    _(mkbox,       MkBox,       MKBOX,       R2,    "rX := make-box rY")       \
    _(boxset,      BoxSet,      BOXSET,      R2,    "box-set! rX rY")          \
    _(boxref,      BoxRef,      BOXREF,      R2,    "rX := box-ref rY")

/** @brief Enumeration of all opcodes */
enum svm_opcode_t {
    #define X(_lower, _title, upper, _fmt, _desc) SVM_OPCODE_##upper,
    SVM_FOREACH_OPCODE(X)
    #undef X
    SVM_OPCODE_UNDEFINED /**^ The value of this opcode is the number of total opcodes */
};

#endif /* ifndef SVM_OPCODES_H */
