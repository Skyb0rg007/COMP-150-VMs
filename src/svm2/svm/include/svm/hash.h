/** @file svm/hash.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Hashtables and hashing functions
 */
#ifndef SVM_HASH_H
#define SVM_HASH_H

#include <svm/config.h>
#include <svm/value.h>

#define key_t int
#define val_t int
#define key_hash(x)  (x)
#define key_eq(a, b) ((a) == (b))
#define kalloc(n)             malloc(n)
#define krealloc(p, old, new) realloc(p, new)
#define kfree(p, old)         free(p)

typedef struct {
    uint32_t n_buckets, size, n_occupied, upper_bound;
    uint32_t *flags;
    key_t *keys;
    val_t *vals;
} svm_hash_t;

/* void svm_hash_init(svm_hash_t *h); */
/* void svm_hash_free(svm_hash_t *h); */
/* void svm_hash_clear(svm_hash_t *h); */
/* uint32_t svm_hash_get(const svm_hash_t *h, key_t key); */
/* int svm_hash_resize(svm_hash_t *h, uint32_t new_n_buckets); */
/* uint32_t svm_hash_put(svm_hash_t *h, key_t key, int *ret); */

static inline size_t svm_hash__fsize(uint32_t m)
{
    return m < 16 ? 1 : m >> 4;
}

static inline bool svm_hash__iseither(uint32_t *flags, uint32_t x)
{
    return (flags[x >> 4] >> ((x & 0xfu) << 1)) & 2;
}

#define svm_hash_exists(h, x) (!svm_hash__iseither((h)->flags, (x)))
#define svm_hash_key(h, x)    ((h)->keys[(x)])
#define svm_hash_val(h, x)    ((h)->vals[(x)])

#define svm_hash_begin(h) ((uint32_t)0)
#define svm_hash_end(h)   ((h)->n_buckets)
#define svm_hash_foreach(h, ivar)                                         \
    for ((ivar) = svm_hash_begin(h); (ivar) != svm_hash_end(h); (ivar)++) \
        if (!svm_hash_exists((h), (ivar)))                                \
            continue;                                                     \
        else

static inline void svm_hash_init(svm_hash_t *h)
{
    memset(h, 0x0, sizeof *h);
}

static inline void svm_hash_free(svm_hash_t *h)
{
    if (h) {
        free(h->keys);
        free(h->flags);
        free(h->vals);
    }
}

static inline void svm_hash_clear(svm_hash_t *h)
{
    if (h && h->flags)
    {
        memset(h->flags, 0xaa, svm_hash__fsize(h->n_buckets) * sizeof(uint32_t));
    }
}

#endif /* ifndef SVM_HASH_H */
