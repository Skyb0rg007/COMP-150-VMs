
#include <svm/config.h>
#include <svm/opcodes.h>
#include <svm/instruction.h>

void svm_instruction_print(svm_instruction_t instr, FILE *outfile)
{
    enum svm_opcode_t opcode = svm_instruction_opcode(instr);
    switch (opcode)
    {
        #define R0(fmt) fprintf(stderr, fmt);
        #define R1(fmt) fprintf(stderr, fmt, svm_instruction_x(instr));
        #define R2(fmt) fprintf(stderr, fmt, svm_instruction_x(instr), svm_instruction_y(instr));
        #define R3(fmt) fprintf(stderr, fmt, svm_instruction_x(instr), svm_instruction_y(instr), svm_instruction_z(instr));
        #define R1LIT(fmt) fprintf(stderr, fmt, svm_instruction_x(instr), svm_instruction_yz(instr));
        #define R0I24(fmt) fprintf(stderr, fmt, svm_instruction_xyz(instr));
        #define R1GLOBAL(fmt) fprintf(stderr, fmt, svm_instruction_x(instr), svm_instruction_yz(instr));
        #define X(lower, _title, upper, format, fmt, _desc, _code) \
            case SVM_OPCODE_##upper:                               \
                format(fmt);                                       \
                break;
        #include <svm/opcode-data.h>
        #undef R0
        #undef R1
        #undef R2
        #undef R3
        #undef R1LIT
        #undef R0I24
        #undef X

        default:
            fprintf(outfile, "undefined instruction: %#x", instr);
    }
}
