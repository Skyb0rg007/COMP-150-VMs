/** @file svm/heap.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Functions for working with heap memory
 *
 * Functions used for gc memory allocation
 */
#ifndef SVM_HEAP_H
#define SVM_HEAP_H

#include <svm/config.h>
#include <svm/vm.h>

extern void svm_heap_init(struct svm_vm_t *vm);
extern void svm_heap_free(struct svm_vm_t *vm);
extern void svm_heap_gc(struct svm_vm_t *vm);
extern void *svm_heap_alloc(struct svm_vm_t *vm, size_t size);

#endif /* ifndef SVM_HEAP_H */
