/** @file svm/string.h
 * Operations for working with allocated strings.
 * The stringtable structure lies in the vm.h file.
 */
#ifndef SVM_STRING_H
#define SVM_STRING_H

#include <svm/config.h>
#include <svm/vm.h>

/* Memory management 
 * These functions should be not be called in user code
 */
extern void svm_stringtable_init(struct svm_vm_t *vm) SVM_ATTR_NONNULL(1);
extern void svm_stringtable_free(struct svm_vm_t *vm) SVM_ATTR_NONNULL(1);
extern void svm_stringtable_gc(struct svm_vm_t *vm) SVM_ATTR_NONNULL(1);

/* String functions */
extern struct svm_string_t *svm_string_new(struct svm_vm_t *vm, const char *str, size_t len) SVM_ATTR_NONNULL(1, 2) SVM_ATTR_RETURNS_NONNULL;
extern bool svm_string_equal(const struct svm_string_t *s1, const struct svm_string_t *s2) SVM_ATTR_NONNULL(1, 2);
extern uint32_t svm_string_hash(struct svm_string_t *s);

#endif /* ifndef SVM_STRING_H */
