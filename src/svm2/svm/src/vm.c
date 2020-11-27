
#include <svm/config.h>
#include <svm/vm.h>
#include <svm/string.h>
#include <svm/heap.h>

void svm_vm_init(struct svm_vm_t *vm)
{
    memset(vm, 0x0, sizeof *vm);
    vm->allocator.fun = svm_default_allocator;
    vm->allocator.ud = NULL;

    vm->literals.size = 0;
    vm->literals.capacity = 0;
    vm->literals.elems = NULL;

    svm_heap_init(vm);
    svm_stringtable_init(vm);
}

void svm_vm_free(struct svm_vm_t *vm)
{
    free(vm->literals.elems);
    svm_stringtable_free(vm);
    svm_heap_free(vm);
    memset(vm, 0x0, sizeof *vm);
}

void svm_vm_gc(struct svm_vm_t *vm)
{
    SVM_UNUSED(vm);
    svm_log("GC: garbage collection not implemented");
}

