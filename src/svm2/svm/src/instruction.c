
#include <svm/instruction.h>

void svm_instruction_print(svm_instruction_t instr, FILE *outfile)
{

    enum svm_opcode_t opcode = svm_instruction_opcode(instr);
    const char *name;
    switch (opcode)
    {
        #define X(lower, _title, upper, format, _desc) \
            case SVM_OPCODE_##upper:                   \
                name = #lower;                         \
                goto format;
        SVM_FOREACH_OPCODE(X)
        #undef X

        default:
            fprintf(outfile, "<undefined: %#x>", instr);
    }

R0:
    fprintf(outfile, "%s", name);
    return;
R1:
    fprintf(outfile, "%s %u", name, svm_instruction_x(instr));
    return;
R2:
    fprintf(outfile, "%s %u %u",
            name, svm_instruction_x(instr), svm_instruction_y(instr));
    return;
R3:
    fprintf(outfile, "%s %u %u %u",
            name, svm_instruction_x(instr), svm_instruction_y(instr), svm_instruction_z(instr));
    return;
R1LIT:
    fprintf(outfile, "%s %u %u",
            name,
            svm_instruction_x(instr),
            svm_instruction_yz(instr));
    return;
R0I24:
    fprintf(outfile, "%s %"PRIu32"\n", name, svm_instruction_xyz(instr));
}
