
#include <svm/config.h>
#include <svm/heap.h>
#include <svm/vector.h>
#include <svm/string.h>

#define PAGESIZE        1200
#define NPAYLOAD        (PAGESIZE - sizeof(struct svm_page_t *) - sizeof(union align))
#define SMALL_OBJ_LIMIT NPAYLOAD

/* Object with max alignment */
union align {
    int i;
    long l;
    long *lp;
    void (*fp)(void);
    float f;
    double d;
    long double ld;
};

/* An allocated page */
struct svm_page_t {
    struct svm_page_t *link;
    union align a;
};

/* Gray stack functions */
static void graystack_init(struct svm_vm_t *vm);
static void graystack_free(struct svm_vm_t *vm);
static void graystack_push(struct svm_vm_t *vm, struct svm_value_t *v);
static struct svm_value_t *graystack_pop(struct svm_vm_t *vm); /* Returns NULL if empty */
/* Page functions */
static void acquire_available_page(struct svm_vm_t *vm);
static void take_available_page(struct svm_vm_t *vm);
static int make_available(struct svm_vm_t *vm, struct svm_page_t *pages);
static int free_pages(struct svm_vm_t *vm, struct svm_page_t *pages);
static int make_invalid_and_stomp(struct svm_vm_t *vm, struct svm_page_t *pages);
static bool on_page_list(struct svm_vm_t *vm, void *p, struct svm_page_t *pages);
static void growheap(struct svm_vm_t *vm, double gamma);
/* Copy functions */
static struct svm_string_t *copy_string(struct svm_vm_t *vm, const struct svm_string_t *);
static struct svm_function_t *copy_function(struct svm_vm_t *vm, const struct svm_function_t *);
static struct svm_activation_t *copy_activation(struct svm_vm_t *vm, const struct svm_activation_t *);
static struct svm_box_t *copy_box(struct svm_vm_t *vm, const struct svm_box_t *);
static struct svm_vector_t *copy_vector(struct svm_vm_t *vm, const struct svm_vector_t *);
static struct svm_cons_t *copy_cons(struct svm_vm_t *vm, const struct svm_cons_t *);
static struct svm_closure_t *copy_closure(struct svm_vm_t *vm, const struct svm_closure_t *);
/* Forwarding functions */
static struct svm_string_t *forward_string(struct svm_vm_t *vm, struct svm_string_t *, bool *changed);
static struct svm_function_t *forward_function(struct svm_vm_t *vm, struct svm_function_t *, bool *changed);
static struct svm_activation_t *forward_activation(struct svm_vm_t *vm, struct svm_activation_t *, bool *changed);
static struct svm_box_t *forward_box(struct svm_vm_t *vm, struct svm_box_t *, bool *changed);
static struct svm_vector_t *forward_vector(struct svm_vm_t *vm, struct svm_vector_t *, bool *changed);
static struct svm_cons_t *forward_cons(struct svm_vm_t *vm, struct svm_cons_t *, bool *changed);
static struct svm_closure_t *forward_closure(struct svm_vm_t *vm, struct svm_closure_t *, bool *changed);
/* Scanning functions */
static void scan_value(struct svm_vm_t *vm, struct svm_value_t *val);
static void scan_activation(struct svm_vm_t *vm, struct svm_activation_t *act);
static void scan_vmstate(struct svm_vm_t *vm);

void svm_heap_init(struct svm_vm_t *vm)
{
    struct svm_heap_t *heap = &vm->heap;
    heap->current = NULL;
    heap->available = NULL;
    heap->next = NULL;
    heap->limit = NULL;
    heap->invalid = NULL;
    heap->availibility_floor = 0;
    memset(&heap->count, 0x0, sizeof heap->count);
    heap->gc_in_progress = false;
    heap->gc_needed = false;
    svm_vector_init(&heap->gray);
    memset(&heap->total, 0x0, sizeof heap->total);

    take_available_page(vm);
}

void svm_heap_free(struct svm_vm_t *vm)
{
    struct svm_heap_t *heap = &vm->heap;
    int ccount = free_pages(vm, heap->current);
    int acount = free_pages(vm, heap->available);
    SVM_UNUSED(ccount);
    SVM_UNUSED(acount);
    /* TODO: why does this fail? */
    /* svm_log("ccount = %d, count.current.pages = %d", */
            /* ccount, heap->count.current.pages); */
    /* svm_assert(ccount == heap->count.current.pages); */
    svm_assert(acount == heap->count.available.pages);

    heap->current = heap->available = NULL;

    free_pages(vm, heap->invalid);
    graystack_free(vm);
}

static void forward_value(struct svm_vm_t *vm, struct svm_value_t *val)
{
    switch (val->tag)
    {
        case SVM_VALUE_TAG_VOID:
        case SVM_VALUE_TAG_BOOLEAN:
        case SVM_VALUE_TAG_CHAR:
        case SVM_VALUE_TAG_NUMBER:
        case SVM_VALUE_TAG_EMPTYLIST:
            break;

        case SVM_VALUE_TAG_STRING: {
            val->rep.as_string = forward_string(vm, val->rep.as_string, NULL);
            break;
        }
        case SVM_VALUE_TAG_FUNCTION: {
            val->rep.as_function = forward_function(vm, val->rep.as_function, NULL);
            break;
        }
        case SVM_VALUE_TAG_CONS: {
            bool changed = false;
            val->rep.as_cons = forward_cons(vm, val->rep.as_cons, &changed);
            if (changed)
                svm_vector_push_back(&vm->heap.gray, val, &vm->allocator);
            break;
        }
        case SVM_VALUE_TAG_VECTOR: {
            bool changed = false;
            val->rep.as_vector = forward_vector(vm, val->rep.as_vector, &changed);
            if (changed)
                svm_vector_push_back(&vm->heap.gray, val, &vm->allocator);
            break;
        }
        case SVM_VALUE_TAG_CLOSURE: {
            bool changed = false;
            val->rep.as_closure = forward_closure(vm, val->rep.as_closure, &changed);
            if (changed)
                svm_vector_push_back(&vm->heap.gray, val, &vm->allocator);
            break;
        }
        case SVM_VALUE_TAG_BOX: {
            bool changed = false;
            val->rep.as_box = forward_box(vm, val->rep.as_box, &changed);
            if (changed)
                svm_vector_push_back(&vm->heap.gray, val, &vm->allocator);
            break;
        }
        case SVM_VALUE_TAG_CONTINUATION: {
            bool changed = false;
            val->rep.as_continuation = forward_activation(vm, val->rep.as_continuation, &changed);
            if (changed)
                svm_vector_push_back(&vm->heap.gray, val, &vm->allocator);
            break;
        }
        default:
            svm_panic("Invalid tag %d", val->tag);
    }
}

static void scan_activation(struct svm_vm_t *vm, struct svm_activation_t *act)
{
    act->fun = forward_function(vm, act->fun, NULL);
    for (int i = 0; i < act->nregs; i++)
        scan_value(vm, &act->regs[i]);
}

static void scan_vmstate(struct svm_vm_t *vm)
{
    /* Roots */
    if (vm->current)
        scan_activation(vm, vm->current);
    struct svm_value_t *x;
    svm_vector_foreach(x, &vm->literals) {
        forward_value(vm, x);
    }
}

static void scan_value(struct svm_vm_t *vm, struct svm_value_t *val)
{
    switch (val->tag)
    {
        case SVM_VALUE_TAG_VOID:
        case SVM_VALUE_TAG_BOOLEAN:
        case SVM_VALUE_TAG_CHAR:
        case SVM_VALUE_TAG_NUMBER:
        case SVM_VALUE_TAG_EMPTYLIST:
            svm_panic("Attempt to scan a non-garbage-collected value");
        case SVM_VALUE_TAG_STRING:
        case SVM_VALUE_TAG_SYMBOL:
        case SVM_VALUE_TAG_FUNCTION:
            svm_panic("Attempt to scan a value with no pointers");
        case SVM_VALUE_TAG_CONS: {
            struct svm_cons_t *cons = val->rep.as_cons;
            forward_value(vm, &cons->car);
            forward_value(vm, &cons->cdr);
            break;
        }
        case SVM_VALUE_TAG_BOX: {
            struct svm_box_t *box = val->rep.as_box;
            forward_value(vm, &box->val);
            break;
        }
        case SVM_VALUE_TAG_VECTOR: {
            struct svm_vector_t *vec = val->rep.as_vector;
            for (size_t i = 0; i < svm_vector_size(&vec->vec); i++)
                forward_value(vm, &svm_vector_at(&vec->vec, i));
            break;
        }
        case SVM_VALUE_TAG_CLOSURE: {
            struct svm_closure_t *closure = val->rep.as_closure;
            closure->fun = forward_function(vm, closure->fun, NULL);
            for (int i = 0; i < closure->nslots; i++)
                forward_value(vm, &closure->slots[i]);
            break;
        }
        case SVM_VALUE_TAG_CONTINUATION: {
            struct svm_activation_t *act = val->rep.as_continuation;
            val->rep.as_continuation = forward_activation(vm, act, NULL);
            break;
        }
    }
}

void svm_heap_gc(struct svm_vm_t *vm)
{
    struct svm_page_t *fromspace = vm->heap.current;
    vm->heap.current = NULL;
    take_available_page(vm);

    vm->heap.gc_in_progress = true;

    scan_vmstate(vm);

    while (!svm_vector_empty(&vm->heap.gray)) {
        struct svm_value_t *val = svm_vector_back(&vm->heap.gray);
        svm_vector_pop_back(&vm->heap.gray);
        scan_value(vm, val);
    }

    svm_stringtable_gc(vm);

    make_available(vm, fromspace);

    double gamma = 2.1;
    growheap(vm, gamma);

    vm->heap.total.collections++;
    vm->heap.gc_in_progress = false;
    vm->heap.gc_needed = false;
}

static size_t roundup(size_t n)
{
    size_t block = sizeof(union align);
    return ((n + block + 1) / block) * block;
}

void *svm_heap_alloc(struct svm_vm_t *vm, size_t size)
{
    struct svm_heap_t *heap = &vm->heap;
    svm_assert(size > 0);
    svm_assert(size <= SMALL_OBJ_LIMIT);
    ptrdiff_t nbytes = roundup(size);
    svm_assert(nbytes < heap->limit - (char *)&heap->current->a);
    if (heap->next + nbytes > heap->limit) {
        take_available_page(vm);
    }
    svm_assert(nbytes <= heap->limit - heap->next);

    void *object = heap->next;
    heap->next += nbytes;

    heap->count.current.objects++;
    heap->count.current.bytes_requested += size;

    if (heap->gc_in_progress) {
        heap->total.bytes_copied += size;
    } else {
        heap->total.allocations++;
        heap->total.bytes_requested += size;
    }

    return object;
}

/*****************************************************************************
 *
 * Copy functions
 *
 ****************************************************************************/

static struct svm_string_t *copy_string(struct svm_vm_t *vm, const struct svm_string_t *src)
{
    size_t size = svm_string_allocsize(src->length + 1);
    struct svm_string_t *dst = svm_heap_alloc(vm, size);
    memcpy(dst, src, size);
    return dst;
}
static struct svm_function_t *copy_function(struct svm_vm_t *vm, const struct svm_function_t *src)
{
    size_t size = svm_function_allocsize(src->size);
    struct svm_function_t *dst = svm_heap_alloc(vm, size);
    memcpy(dst, src, size);
    return dst;
}
static struct svm_activation_t *copy_activation(struct svm_vm_t *vm, const struct svm_activation_t *src)
{
    size_t size = svm_function_allocsize(src->nregs);
    struct svm_activation_t *dst = svm_heap_alloc(vm, size);
    memcpy(dst, src, size);
    return dst;
}
static struct svm_box_t *copy_box(struct svm_vm_t *vm, const struct svm_box_t *src)
{
    struct svm_box_t *dst = svm_heap_alloc(vm, sizeof *dst);
    memcpy(dst, src, sizeof *dst);
    return dst;
}
static struct svm_vector_t *copy_vector(struct svm_vm_t *vm, const struct svm_vector_t *src)
{
    struct svm_vector_t *dst = svm_heap_alloc(vm, sizeof *dst);
    memcpy(dst, src, sizeof *dst);
    return dst;
}
static struct svm_cons_t *copy_cons(struct svm_vm_t *vm, const struct svm_cons_t *src)
{
    struct svm_cons_t *dst = svm_heap_alloc(vm, sizeof *dst);
    memcpy(dst, src, sizeof *dst);
    return dst;
}
static struct svm_closure_t *copy_closure(struct svm_vm_t *vm, const struct svm_closure_t *src)
{
    size_t size = svm_closure_allocsize(src->nslots);
    struct svm_closure_t *dst = svm_heap_alloc(vm, size);
    memcpy(dst, src, size);
    return dst;
}

/*****************************************************************************
 *
 * Forwarding functions
 *
 ****************************************************************************/

static struct svm_string_t *forward_string(struct svm_vm_t *vm, struct svm_string_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_string(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}
static struct svm_function_t *forward_function(struct svm_vm_t *vm, struct svm_function_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_function(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}
static struct svm_activation_t *forward_activation(struct svm_vm_t *vm, struct svm_activation_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_activation(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}
static struct svm_box_t *forward_box(struct svm_vm_t *vm, struct svm_box_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_box(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}
static struct svm_vector_t *forward_vector(struct svm_vm_t *vm, struct svm_vector_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_vector(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}
static struct svm_cons_t *forward_cons(struct svm_vm_t *vm, struct svm_cons_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_cons(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}
static struct svm_closure_t *forward_closure(struct svm_vm_t *vm, struct svm_closure_t *src, bool *changed)
{
    if (!src->forwarded) {
        src->forwarded = copy_closure(vm, src);
        if (changed)
            *changed = true;
    }
    return src->forwarded;
}

/*****************************************************************************
 *
 * Graystack functions
 *
 ****************************************************************************/

static void graystack_init(struct svm_vm_t *vm)
{
    struct svm_heap_t *heap = &vm->heap;
    size_t initial_cap = 40;
    svm_vector_init(&heap->gray);
    svm_vector_reserve(&heap->gray, initial_cap, &vm->allocator);
}
static void graystack_free(struct svm_vm_t *vm)
{
    struct svm_heap_t *heap = &vm->heap;
    svm_vector_free(&heap->gray, &vm->allocator);
}
static void graystack_push(struct svm_vm_t *vm, struct svm_value_t *v)
{
    struct svm_heap_t *heap = &vm->heap;
    svm_vector_push_back(&heap->gray, v, &vm->allocator);
}
static struct svm_value_t *graystack_pop(struct svm_vm_t *vm)
{
    struct svm_heap_t *heap = &vm->heap;
    if (svm_vector_empty(&heap->gray))
        return NULL;
    struct svm_value_t *x = svm_vector_back(&heap->gray);
    svm_vector_pop_back(&heap->gray);
    return x;
}

/*****************************************************************************
 *
 * Page functions
 *
 ****************************************************************************/

static void acquire_available_page(struct svm_vm_t *vm)
{
    struct svm_page_t *page = svm_alloc(&vm->allocator, PAGESIZE);
    page->link = vm->heap.available;
    vm->heap.available = page;
    vm->heap.count.available.pages++;
}

static struct svm_page_t *newpage(struct svm_vm_t *vm)
{
    if (vm->heap.available == NULL) {
        acquire_available_page(vm);
    }
    svm_assert_paranoid(vm->heap.available);
    struct svm_page_t *page = vm->heap.available;
    vm->heap.available = vm->heap.available->link;
    vm->heap.count.available.pages--;
    if (vm->heap.count.available.pages < vm->heap.availibility_floor) {
        vm->heap.gc_needed = true;
    }
    return page;
}

static void take_available_page(struct svm_vm_t *vm)
{
    struct svm_page_t *page = newpage(vm);
    page->link = vm->heap.current;
    vm->heap.current = page;
    vm->heap.count.current.pages++;
    vm->heap.next = (char *)&vm->heap.current->a;
    vm->heap.limit = (char *)vm->heap.current + PAGESIZE;
}

static int make_available(struct svm_vm_t *vm, struct svm_page_t *pages)
{
    int reclaimed = 0;
    while (pages)
    {
        struct svm_page_t *p = pages;
        pages = p->link;
        p->link = vm->heap.available;
        vm->heap.available = p;
        reclaimed++;
        vm->heap.count.available.pages++;
    }
    return reclaimed;
}

static int free_pages(struct svm_vm_t *vm, struct svm_page_t *pages)
{
    int count = 0;
    while (pages)
    {
        count++;
        struct svm_page_t *next = pages->link;
        svm_free(&vm->allocator, pages, PAGESIZE);
        pages = next;
    }
    return count;
}

static int make_invalid_and_stomp(struct svm_vm_t *vm, struct svm_page_t *pages)
{
    int invalidated = 0;
    while (pages)
    {
        struct svm_page_t *p = pages;
        pages = p->link;
        p->link = vm->heap.invalid;
        vm->heap.invalid = p;
        memset(p + 1, 0xff, PAGESIZE - sizeof *p);
        invalidated++;
    }
    return invalidated;
}

static bool on_page_list(struct svm_vm_t *vm, void *p, struct svm_page_t *page)
{
    (void)vm;
    for (; page; page = page->link)
    {
        char *first = (char *)&page->a;
        char *limit = (char *)page + PAGESIZE;
        char *test = p;
        if (first <= test && test < limit)
            return true;
    }
    return false;
}

static void growheap(struct svm_vm_t *vm, double gamma)
{
    bool grew = false;
    struct svm_heap_t *heap = &vm->heap;
    while (PAGESIZE * (heap->count.current.pages * heap->count.available.pages)
            < heap->count.current.bytes_requested * gamma) {
        grew = true;
        acquire_available_page(vm);
    }
    if (grew) {
        svm_log("Grew heap to %d pages\n", heap->count.current.pages + heap->count.available.pages);
    }
}



