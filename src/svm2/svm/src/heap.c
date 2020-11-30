
#include <svm/config.h>
#include <svm/heap.h>

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
    svm_assert(ccount == heap->count.current.pages);
    svm_assert(acount == heap->count.available.pages);

    heap->current = heap->available = NULL;

    free_pages(vm, heap->invalid);
    graystack_free(vm);
}

void svm_heap_gc(struct svm_vm_t *vm)
{
    SVM_UNUSED(vm);
    /* TODO: implement */
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
