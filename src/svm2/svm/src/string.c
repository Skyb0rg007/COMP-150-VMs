
#include <svm/config.h>
#include <svm/string.h>
#include <svm/value.h>
#include <svm/vm.h>
#include <svm/heap.h>

#define is_shortlen(len) ((len) <= SVM_STRING_MAX_SHORTLEN)
#define is_longlen(len)  ((len) > SVM_STRING_MAX_SHORTLEN)
#define is_short(s) is_shortlen((s)->length)
#define is_long(s)  is_longlen((s)->length)
#define step_size(len) (((len) >> 5) + 1)

void svm_stringtable_init(struct svm_vm_t *vm)
{
    struct svm_stringtable_t *tab = &vm->stringtable;
    tab->population = 0;
    tab->nbuckets = SVM_STRING_MIN_STRTAB;
    tab->buckets = calloc(SVM_STRING_MIN_STRTAB, sizeof(*tab->buckets));
    svm_assert(tab->buckets);
}

void svm_stringtable_free(struct svm_vm_t *vm)
{
    struct svm_stringtable_t *tab = &vm->stringtable;
    tab->population = 0;
    tab->nbuckets = 0;
    free(tab->buckets);
    tab->buckets = NULL;
}

static struct svm_string_t *keep_alive(struct svm_string_t *s);
void svm_stringtable_gc(struct svm_vm_t *vm)
{
    struct svm_stringtable_t *tab = &vm->stringtable;
    for (int i = 0; i < tab->nbuckets; i++) {
        tab->buckets[i] = keep_alive(tab->buckets[i]);
    }
}

static struct svm_string_t *keep_alive(struct svm_string_t *s)
{
    while (s && s->forwarded == NULL) {
        s = s->next_interned;
    }

    if (s) {
        s->forwarded->next_interned = keep_alive(s->next_interned);
        return s->forwarded;
    } else {
        return NULL;
    }
}

static uint32_t compute_hash(const char *str, size_t len)
{
    const uint32_t seed = 0x00beef00;
    uint32_t h = seed ^ len;
    size_t step = step_size(len);
    for (; len >= step; len -= step) {
        h ^= ((h << 5) + (h >> 2) + (str[len - 1]));
    }
    if (h == 0)
        h = 0xfeeb;
    return h;
}

static void resize(struct svm_vm_t *vm, int newsize)
{
    struct svm_stringtable_t *tb = &vm->stringtable;
    if (newsize > tb->nbuckets) {  /* grow table if needed */
        tb->buckets = realloc(tb->buckets, newsize * sizeof(tb->buckets[0]));
        /* vulnerable to arithmetic overflow in multiplication */
        for (int i = tb->nbuckets; i < newsize; i++)
            tb->buckets[i] = NULL;
    }
    for (int i = 0; i < tb->nbuckets; i++) {  /* rehash */
        struct svm_string_t *p = tb->buckets[i];
        tb->buckets[i] = NULL;
        while (p) {  /* for each node in the list */
            struct svm_string_t *next = p->next_interned;  /* save next */
            unsigned int h = p->hash % newsize;  /* new position */
            p->next_interned = tb->buckets[h];  /* chain it */
            tb->buckets[h] = p;
            p = next;
        }
    }
    if (newsize < tb->nbuckets) {  /* shrink table if needed */
        /* vanishing slice should be empty */
        svm_assert(tb->buckets[newsize] == NULL && tb->buckets[tb->nbuckets - 1] == NULL);
        tb->buckets = realloc(tb->buckets, newsize * sizeof(tb->buckets[0]));
    }
    tb->nbuckets = newsize;
}

static struct svm_string_t *allocstring(struct svm_vm_t *vm, size_t len, size_t hash)
{
    struct svm_string_t *s = svm_heap_alloc(vm, svm_string_allocsize(len + 1));
    s->forwarded = NULL;
    s->length = len;
    s->hash = hash;
    s->next_interned = NULL;
    s->bytes[0] = '\0';
    return s;
}

static struct svm_string_t *intern_short(
        struct svm_vm_t *vm,
        const char *str,
        size_t len,
        struct svm_string_t *alloced)
{
    struct svm_stringtable_t *tb = &vm->stringtable;
    svm_assert(!is_longlen(len));
    uint32_t h = compute_hash(str, len);
    struct svm_string_t **list = &tb->buckets[h % tb->nbuckets];
    assert(str != NULL);  /* otherwise 'memcmp'/'memcpy' are undefined */
    for (struct svm_string_t *hs = *list;
            hs != NULL;
            hs = hs->next_interned) {
        if (len == hs->length &&
                (memcmp(str, hs->bytes, len * sizeof(char)) == 0)) {
            /* found! */
            /* if dead, might have to mark it live for GC */
            return hs;
        }
    }
    if (tb->population >= tb->nbuckets && tb->nbuckets <= INT_MAX/2) {
        resize(vm, tb->nbuckets * 2);
        list = &tb->buckets[h % tb->nbuckets];  /* recompute with new size */
    }
    struct svm_string_t *hs;
    if (alloced != NULL) {
        hs = alloced;
        hs->hash = h;
    } else {
        hs = allocstring(vm, len, h);
        memcpy(hs->bytes, str, len);
        hs->bytes[len] = '\0';
    }
    hs->next_interned = *list;
    *list = hs;
    tb->population++;
    return hs;
}

struct svm_string_t *svm_string_new(struct svm_vm_t *vm, const char *str, size_t len)
{
    if (is_longlen(len)) {
        struct svm_string_t *s;
        svm_assert(len < (SIZE_MAX - sizeof *s / sizeof(char)));
        s = allocstring(vm, len, 0);
        memcpy(s->bytes, str, len);
        s->bytes[len] = '\0';
        return s;
    } else {
        return intern_short(vm, str, len, NULL);
    }
}

bool svm_string_equal(
        struct svm_vm_t *vm,
        const struct svm_string_t *s1,
        const struct svm_string_t *s2)
{
    (void)vm;
    if (s1 == s2)
        return true;
    if (is_short(s1) || is_short(s2))
        /* Short strings are equal iff they are pointer equal */
        return false;
    if (s1->length != s2->length)
        return false;
    return memcmp(s1->bytes, s2->bytes, s1->length) == 0;
}

uint32_t svm_string_hash(struct svm_vm_t *vm, struct svm_string_t *s)
{
    (void)vm;
    if (s->hash == 0)
        s->hash = compute_hash(s->bytes, s->length);
    return s->hash;
}

