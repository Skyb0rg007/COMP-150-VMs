
#ifndef SVM_H
#define SVM_H

#include "svmconfig.h"

/** @brief Allocator type */
typedef void *(*svm_alloc_t)(void *p, size_t oldsize, size_t newsize, void *ud);
/** @brief A panic function */
typedef void (*svm_panic_t)(const char *reason);

/*****************************************************************************
 *
 * The VM state
 *
 ****************************************************************************/

/** @brief The VM state */
struct svm_vm_t;

/** @brief Allocate a new vm */
extern struct svm_vm_t *svm_newstate(svm_alloc_t fun, void *ud);
/** @brief Free the vm memory */
extern void svm_close(struct svm_vm_t *vm);
/** @brief Set the panic function, returning the old one */
extern svm_panic_t svm_atpanic(struct svm_vm_t *vm, svm_panic_t panicf);
/** @brief Get the allocation function and userdata */
extern svm_alloc_t svm_getallocf(struct svm_vm_t *vm, void **ud);
/** @brief Set the allocation function and userdata
 * @note The new allocator should be compatible with the previous allocator
 */
extern void svm_setallocf(struct svm_vm_t *vm, svm_alloc_t f, void *ud);
/** @brief Run the VM on the given file */
extern void svm_dofile(struct svm_vm_t *vm, FILE *fp);

/*****************************************************************************
 *
 * Strings
 *
 ****************************************************************************/

/** @brief VM string type */
struct svm_string_t;

/** @brief Construct a string */
extern struct svm_string_t *svm_string_new(struct svm_vm_t *vm, const char *str, size_t len);
/** @brief Query the string length */
extern size_t svm_string_length(const struct svm_string_t *str);
/** @brief Query the string data */
extern const char *svm_string_data(const struct svm_string_t *str);
/** @brief String equality */
extern bool svm_string_equal(const struct svm_string_t *a, const struct svm_string_t *b);
/** @brief String hashing
 * @note String is not const since hash is cached
 */
extern uint32_t svm_string_hash(struct svm_string_t *a);

#endif /* ifndef SVM_H */
