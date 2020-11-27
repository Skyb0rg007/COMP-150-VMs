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

/** @brief An instruction is tightly packed into a 32-bit integer */
typedef uint32_t svm_instruction_t;

/** @brief Access the opcode component of an instruction */
SVM_ATTR_CONST
static inline enum svm_opcode_t svm_instruction_opcode(svm_instruction_t i)
{
    return (i >> 24) & 0xff;
}
/** @brief Access the x component of an instruction */
SVM_ATTR_CONST
static inline uint8_t svm_instruction_x(svm_instruction_t i)
{
    return (i >> 16) & 0xff;
}
/** @brief Access the y component of an instruction */
SVM_ATTR_CONST
static inline uint8_t svm_instruction_y(svm_instruction_t i)
{
    return (i >> 8) & 0xff;
}
/** @brief Access the z component of an instruction */
SVM_ATTR_CONST
static inline uint8_t svm_instruction_z(svm_instruction_t i)
{
    return i & 0xff;
}
/** @brief Access the yz component of an instruction */
SVM_ATTR_CONST
static inline uint16_t svm_instruction_yz(svm_instruction_t i)
{
    return i & 0xffff;
}
/** @brief Access the xyz component of an instruction */
SVM_ATTR_CONST
static inline int32_t svm_instruction_xyz(svm_instruction_t i)
{
    return i & 0xffffff;
}

/** @brief Construct an instruction that takes 3 arguments */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r3(enum svm_opcode_t op, unsigned x, unsigned y, unsigned z)
{
    svm_assert((op & 0xff) == op);
    svm_assert((x & 0xff) == x);
    svm_assert((y & 0xff) == y);
    svm_assert((z & 0xff) == z);
    return op << 24 | x << 16 | y << 8 | z;
}
/** @brief Construct an instruction that takes 2 arguments */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r2(enum svm_opcode_t op, unsigned x, unsigned y)
{
    return svm_instruction_r3(op, x, y, 0);
}
/** @brief Construct an instruction that takes 1 argument */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r1(enum svm_opcode_t op, unsigned x)
{
    return svm_instruction_r3(op, x, 0, 0);
}
/** @brief Construct an instruction that takes 0 arguments */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r0(enum svm_opcode_t op)
{
    return svm_instruction_r3(op, 0, 0, 0);
}
/** @brief Construct an instruction that takes 2 arguments, one being a literal */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r1lit(enum svm_opcode_t op, unsigned x, unsigned yz)
{
    svm_assert((op & 0xff) == op);
    svm_assert((x & 0xff) == x);
    svm_assert((yz & 0xffff) == yz);
    return op << 24 | x << 16 | yz;
}
/** @brief Construct an instruction that takes 1 24-bit integer argument */
SVM_ATTR_CONST
static inline svm_instruction_t svm_instruction_r0i24(enum svm_opcode_t op, int xyz)
{
    svm_assert((op & 0xff) == op);
    svm_assert((xyz & 0xffffff) == xyz);
    return op << 24 | xyz;
}

extern void svm_instruction_print(svm_instruction_t instr, FILE *outfile);

#endif /* ifndef SVM_INSTRUCTION_H */
