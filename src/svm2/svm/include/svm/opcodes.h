#ifndef SVM_OPCODES_H
#define SVM_OPCODES_H

#include <svm/config.h>

enum svm_opcode_t {
    #define X(_lower, _title, upper, _fmt, _, _desc, _code) SVM_OPCODE_##upper,
    #include <svm/opcode-data.h>
    #undef X
    SVM_OPCODE_UNDEFINED /* The value of this enum is the number of total opcodes */
};

#endif /* ifndef SVM_OPCODES_H */
