/** @file svm/heap.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Functions for working with memory
 * Functions used for both gc and non-gc memory allocation
 */
#ifndef SVM_HEAP_H
#define SVM_HEAP_H

#include <svm/config.h>
#include <svm/vm.h>

/** @brief Allocate non-garbage collected memory */
SVM_ATTR_NONNULL(1) SVM_ATTR_WARN_UNUSED_RESULT
static inline void *svm_alloc(struct svm_vm_t *vm, size_t size)
{
    return vm->allocator.fun(NULL, 0, size, vm->allocator.ud);
}

/** @brief Free non-garbage collected memory */
SVM_ATTR_NONNULL(1)
static inline void svm_free(struct svm_vm_t *vm, void *p, size_t size)
{
    (void)vm->allocator.fun(p, size, 0, vm->allocator.ud);
}

/** @brief Reallocate non-garbage collected memory */
SVM_ATTR_NONNULL(1) SVM_ATTR_WARN_UNUSED_RESULT
static inline void *svm_realloc(struct svm_vm_t *vm, void *p, size_t oldsize, size_t newsize)
{
    return vm->allocator.fun(p, oldsize, newsize, vm->allocator.ud);
}

/** @brief The default memory allocator for non-garbage collected memory */
extern void *svm_default_allocator(void *p, size_t oldsz, size_t newsz, void *ud);

/** @brief Initialize the heap. Should not be called by user code. */
extern void svm_heap_init(struct svm_vm_t *vm);
/** @brief Free the heap. Should not be called by user code. */
extern void svm_heap_free(struct svm_vm_t *vm);
/** @brief Run a gc step */
extern void svm_heap_gc(struct svm_vm_t *vm);
/** @brief Allocate garbage-collected memory */
extern void *svm_gc_alloc(struct svm_vm_t *vm, size_t size);

#endif /* ifndef SVM_HEAP_H */
