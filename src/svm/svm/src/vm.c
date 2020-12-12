
#include <svm/config.h>
#include <svm/vm.h>
#include <svm/string.h>
#include <svm/heap.h>
#include <svm/vector.h>

void svm_vm_init(struct svm_vm_t *vm)
{
    memset(vm, 0x0, sizeof *vm);

    /* This must be done first */
    vm->allocator.fun = svm_default_allocator;
    vm->allocator.ud = NULL;

    /* For check-expect */
    vm->checks.ntests = 0;
    vm->checks.npassed = 0;
    vm->checks.source = NULL;
    vm->checks.value = NULL;

    svm_vector_init(&vm->literals);
    svm_vector_init(&vm->globals);
    svm_heap_init(vm);
    svm_stringtable_init(vm);
}

void svm_vm_free(struct svm_vm_t *vm)
{
    svm_vector_free(&vm->literals, &vm->allocator);
    svm_vector_free(&vm->globals, &vm->allocator);
    svm_stringtable_free(vm);
    svm_heap_free(vm);
    memset(vm, 0x0, sizeof *vm);
}

void svm_vm_gc(struct svm_vm_t *vm)
{
    svm_heap_gc(vm);
}

