structure AssemblyCode = struct
  datatype instr
    = LOADER of ObjectCode.instr
    | LOADFUNC of ObjectCode.reg * int * instr list
    | IF_GOTO_LABEL of ObjectCode.reg * string
    | GOTO_LABEL of string
    | LABEL of string
end
