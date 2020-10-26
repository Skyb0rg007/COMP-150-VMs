// Defines all the opcodes used in the VM

// When you're thinking about new instructions, define them here first.
// It's OK for an opcode to be defined here even if it is not implemented 
// anywhere.  But if you want to *run* an instruction (module 1) or *load*
// an instruction (module 2), the opcode has to be defined here first.

#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

typedef enum opcode { 
    Halt,        // R0    -- return;
    Print,       // R1    -- print(R(X))
    Check,       // R1LIT -- check(R(X), L(YZ))
    Expect,      // R1LIT -- expect(R(X), L(YZ))
    Add,         // R3    -- R(X) := R(Y) + R(Z)
    LoadLiteral, // R1LIT -- R(X) := L(YZ)
    Goto,        // R0I24 -- ip += XYZ
    If,          // R1    -- if (truthy(R(X))) { ip++; }
    GetGlobal,   // R1LIT -- R(X) := _G[LIT]
    SetGlobal,   // R1LIT -- _G[LIT] := R(X)
    Divide,      // R3    -- R(X) := R(Y) / R(Z)
    Subtract,    // R3    -- R(X) := R(Y) - R(Z)
    Multiply,    // R3    -- R(X) := R(Y) * R(Z)
    Abs,         // R2    -- R(X) := |R(Y)|
    Hash,        // R2    -- R(X) := hash(R(Y))
    CopyReg,     // R2    -- R(X) := R(Y)
    Call,
    Return,
    Tailcall,
    Error,
    TestEq,
    Unimp,       // stand-in for opcodes not yet implemented
                 // used to query number of opcodes, so must be last
} Opcode;


#endif
