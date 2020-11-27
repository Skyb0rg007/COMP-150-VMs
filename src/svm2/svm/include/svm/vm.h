#ifndef SVM_VM_H
#define SVM_VM_H

#include <svm/config.h>
#include <svm/assert.h>
#include <svm/value.h>
/* #include <svm/heap.h> */

/* Holds info about string interning 
 * Used by the string module
 */
struct svm_stringtable_t {
    int population;
    int nbuckets;
    struct svm_string_t **buckets;
};

/* Heap info
 * Used by the heap module
 */
struct svm_heap_t {
    /* Current page
     * Linked to older pages that have already been used for allocation
     */
    struct svm_page_t *current;
    /* Empty pages that can be used for allocation
     */
    struct svm_page_t *available;
    /* Pointers into the current page
     */
    char *next, *limit;
    /* Invalid pages */
    struct svm_page_t *invalid;
    /* When count.available.pages < availibility_floor,
     * a gc round is needed
     */
    int availibility_floor;
    /* Data */
    struct {
        struct {
            int pages;
            /* Num objects allocated */
            int objects;
            /* Num bytes requested for those objects */
            int bytes_requested;
        } current;
        struct {
            int pages;
        } available;
    } count;
    /* Controls accounting of requests */
    bool gc_in_progress;
    /* Signals to vm to call gc() at next safe point */
    bool gc_needed;
    /* Gray objects, stored as a stack
     * Stored in the gc state to reuse the allocated memory
     */
    struct {
        size_t size, capacity;
        struct svm_value_t **elems;
    } gray;
    /* Stats */
    struct {
        int allocations,
            bytes_requested,
            bytes_copied,
            collections;
    } total;
};

/* An activation frame
 * These are garbage collected and organized in a Spaghetti stack
 */
struct svm_activation_t {
    SVM_GC_META(svm_activation_t);
    struct svm_activation_t *parent;
    struct svm_function_t *fun;
    int instr;   /* Instruction counter */
    int destreg; /* Destination register (where to store result in parent) */
    /* Store locals in the frame itself
     * We cannot use register windows because we support call/cc
     */
    int nregs;
    struct svm_value_t regs[];
};

static size_t svm_activation_allocsize(int nregs)
{
    return sizeof(struct svm_activation_t) + nregs * sizeof(struct svm_value_t);
}

/* The entire VM state lies here */
struct svm_vm_t {
    /* Points to executing activation. This is a GC root. */
    struct svm_activation_t *current;

    /* Used by memory allocation functions */
    struct svm_allocator_t {
        void *(*fun)(void *p, size_t oldsize, size_t newsize, void *ud);
        void *ud;
    } allocator;

    /* Holds literals + functions. Not GCed. */
    struct {
        size_t size, capacity;
        struct svm_value_t *elems;
    } literals;

    /* Used by string module to allocate strings. */
    struct svm_stringtable_t stringtable;
    /* Used by heap module to manage memory. */
    struct svm_heap_t heap;
};

extern void svm_vm_init(struct svm_vm_t *vm) SVM_ATTR_NONNULL(1);
extern void svm_vm_free(struct svm_vm_t *vm) SVM_ATTR_NONNULL(1);
extern void svm_vm_gc(struct svm_vm_t *vm) SVM_ATTR_NONNULL(1);

SVM_ATTR_NONNULL(1)
static inline struct svm_value_t svm_vm_loadliteral(struct svm_vm_t *vm, size_t idx)
{
    if (idx > vm->literals.size) {
        svm_panic("Literal index %zu out of bounds (should be less than %zu)",
                idx, vm->literals.size);
    }
    return vm->literals.elems[idx];
}

SVM_ATTR_NONNULL(1)
static inline int svm_vm_pushliteral(struct svm_vm_t *vm, struct svm_value_t value)
{
    if (vm->literals.size == vm->literals.capacity) {
        size_t oldcap = vm->literals.capacity;
        size_t newcap = oldcap ? oldcap * 2 : 128;
        /* void *p = svm_realloc(vm, vm->literals.elems, sizeof vm->literals.elems[0] * oldcap, sizeof vm->literals.elems[0] * newcap); */
        void *p = realloc(vm->literals.elems, sizeof vm->literals.elems[0] * newcap);
        if (p == NULL)
            svm_panic("Attempt to realloc literal pool (%zu -> %zu) failed",
                    oldcap, newcap);
        vm->literals.elems = p;
        vm->literals.capacity = newcap;
    }
    vm->literals.elems[vm->literals.size] = value;
    return vm->literals.size++;
}

#endif /* ifndef SVM_VM_H */
