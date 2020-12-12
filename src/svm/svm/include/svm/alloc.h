/** @file svm/alloc.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Custom allocation primitives
 *
 * Inspired by Lua's custom allocation API
 */
#ifndef SVM_ALLOC_H
#define SVM_ALLOC_H

#include <svm/config.h>

struct svm_allocator_t {
    void *(*fun)(void *p, size_t oldsize, size_t newsize, void *ud);
    void *ud;
};

SVM_ATTR_NONNULL(1) SVM_ATTR_WARN_UNUSED_RESULT
static inline void *svm_alloc(struct svm_allocator_t *alloc, size_t size)
{
    return alloc->fun(NULL, 0, size, alloc->ud);
}

SVM_ATTR_NONNULL(1) SVM_ATTR_WARN_UNUSED_RESULT
static inline void *svm_realloc(struct svm_allocator_t *alloc, void *p, size_t oldsize, size_t newsize)
{
    return alloc->fun(p, oldsize, newsize, alloc->ud);
}

SVM_ATTR_NONNULL(1, 2)
static inline void svm_free(struct svm_allocator_t *alloc, void *p, size_t size)
{
    (void)alloc->fun(p, size, 0, alloc->ud);
}

extern void *svm_default_allocator(void *p, size_t oldsz, size_t newsz, void *ud);

#endif /* ifndef SVM_ALLOC_H */
