/** @file svm/instruction.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Functions for working with SVM instructions
 */
#ifndef SVM_INSTRUCTION_H
#define SVM_INSTRUCTION_H

#include <svm/config.h>
#include <svm/opcodes.h>
#include <svm/assert.h>

typedef uint32_t svm_instruction_t;

/* Accessors */
SVM_ATTR_CONST
static inline enum svm_opcode_t svm_instruction_opcode(svm_instruction_t i)
{
    return (i >> 24) & 0xff;
}
SVM_ATTR_CONST
static inline uint8_t svm_instruction_x(svm_instruction_t i)
{
    return (i >> 16) & 0xff;
}
SVM_ATTR_CONST
static inline uint8_t svm_instruction_y(svm_instruction_t i)
{
    return (i >> 8) & 0xff;
}
SVM_ATTR_CONST
static inline uint8_t svm_instruction_z(svm_instruction_t i)
{
    return i & 0xff;
}
SVM_ATTR_CONST
static inline uint16_t svm_instruction_yz(svm_instruction_t i)
{
    return i & 0xffff;
}
SVM_ATTR_CONST
static inline int32_t svm_instruction_xyz(svm_instruction_t i)
{
    return ((int32_t)i << 8) >> 8;
}

/* Constructors */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r3(enum svm_opcode_t op, unsigned x, unsigned y, unsigned z)
{
    svm_assert((op & 0xff) == op);
    svm_assert((x & 0xff) == x);
    svm_assert((y & 0xff) == y);
    svm_assert((z & 0xff) == z);
    return op << 24 | x << 16 | y << 8 | z;
}
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r2(enum svm_opcode_t op, unsigned x, unsigned y)
{
    return svm_instruction_r3(op, x, y, 0);
}
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r1(enum svm_opcode_t op, unsigned x)
{
    return svm_instruction_r3(op, x, 0, 0);
}
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r0(enum svm_opcode_t op)
{
    return svm_instruction_r3(op, 0, 0, 0);
}
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r1lit(enum svm_opcode_t op, unsigned x, unsigned yz)
{
    svm_assert((op & 0xff) == op);
    svm_assert((x & 0xff) == x);
    svm_assert((yz & 0xffff) == yz);
    return op << 24 | x << 16 | yz;
}
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r0i24(enum svm_opcode_t op, int xyz)
{
    svm_assert((op & 0xff) == op);
    svm_assert((xyz << 8) >> 8 == xyz);
    return op << 24 | (unsigned)(xyz << 8) >> 8;
}

/* Misc */
extern void svm_instruction_print(svm_instruction_t instr, FILE *outfile);

#endif /* ifndef SVM_INSTRUCTION_H */
