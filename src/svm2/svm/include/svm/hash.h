/** @file svm/hash.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Hashtables and hashing functions
 */
#ifndef SVM_HASH_H
#define SVM_HASH_H

#include <svm/config.h>
#include <svm/value.h>

#define SVM_HASH_UPPER 0.77

#define key_t int
#define val_t int
#define key_hash(x)           (x)
#define key_eq(a, b)          ((a) == (b))

typedef struct {
    uint32_t n_buckets, size, n_occupied, upper_bound;
    uint32_t *flags;
    key_t *keys;
    val_t *vals;
} svm_hash_t;

/* Private helper functions */

static inline bool svm_hash__isempty(uint32_t *flags, uint32_t i)
{
    return (flags[i >> 4] >> ((i & 0xfu) << 1)) & 2;
}
static inline bool svm_hash__isdel(uint32_t *flags, uint32_t i)
{
    return (flags[i >> 4] >> ((i & 0xfu) << 1)) & 1;
}
static inline bool svm_hash__iseither(uint32_t *flags, uint32_t i)
{
    return (flags[i >> 4] >> ((i & 0xfu) << 1)) & 3;
}
static inline void svm_has__set_isdel_false(uint32_t *flags, uint32_t i)
{
    flags[i >> 4] &= ~(1ul << ((i & 0xfu) << 1));
}
static inline void svm_hash__set_isempty_false(uint32_t *flags, uint32_t i)
{
    flags[i >> 4] &= ~(2ul << ((i & 0xfu) << 1));
}
static inline void svm_hash__set_isboth_false(uint32_t *flags, uint32_t i)
{
    flags[i >> 4] &= ~(3ul << ((i & 0xfu) << 1));
}
static inline void svm_hash__set_isdel_true(uint32_t *flags, uint32_t i)
{
    flags[i >> 4] |= 1ul << ((i & 0xfu) << 1);
}
static inline size_t svm_hash__fsize(uint32_t m)
{
    return m < 16 ? 1 : m >> 4;
}
static inline uint32_t svm_hash__roundup(uint32_t x)
{
    --x;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return ++x;
}

#define svm_hash_exists(h, x) (!svm_hash__iseither((h)->flags, (x)))
#define svm_hash_key(h, x)    ((h)->keys[(x)])
#define svm_hash_val(h, x)    ((h)->vals[(x)])
#define svm_hash_size(h)      ((h)->size)
#define svm_hash_n_buckets(h) ((h)->n_buckets)

#define svm_hash_begin(h) ((uint32_t)0)
#define svm_hash_end(h)   ((h)->n_buckets)
#define svm_hash_foreach(i, key, val, h)                                              \
    for ((i) = svm_hash_begin(h); (i) != svm_hash_end(h); (i)++)                      \
        if ((svm_hash_exists((h), (i))                                                \
                    ? ((key) = svm_hash_key(h, i), (val) = svm_hash_val(h, i), false) \
                    : true))                                                          \
            continue;                                                                 \
        else

static inline void svm_hash_init(svm_hash_t *h)
{
    memset(h, 0x0, sizeof *h);
    h->n_buckets = h->size = h->n_occupied = h->upper_bound = 0;
    h->flags = NULL;
    h->keys = NULL;
    h->vals = NULL;
}

static inline void svm_hash_free(svm_hash_t *h, struct svm_allocator_t *alloc)
{
    if (h) {
        svm_free(alloc, h->keys, 0);
        svm_free(alloc, h->flags, 0);
        svm_free(alloc, h->vals, 0);
    }
}

static inline void svm_hash_clear(svm_hash_t *h)
{
    if (h && h->flags) {
        memset(h->flags, 0xaa, svm_hash__fsize(h->n_buckets) * sizeof(uint32_t));
        h->size = h->n_occupied = 0;
    }
}

static inline int svm_hash_get(svm_hash_t *h, key_t key)
{
    if (!h->n_buckets) {
        return 0;
    }

    uint32_t k, i, last, mask, step;
    step = 0;
    mask = h->n_buckets - 1;
    k = key_hash(key);
    last = i = k & mask;
    while (!svm_hash__isempty(h->flags, i) && (svm_hash__isdel(h->flags, i) || !key_eq(h->keys[i], key)))
    {
        i = (i + (++step)) & mask;
        if (i == last)
            return h->n_buckets;
    }
    return svm_hash__iseither(h->flags, i)
        ? h->n_buckets
        : i;
}

static inline void svm_hash_resize(svm_hash_t *h, uint32_t new_n_buckets, struct svm_allocator_t *alloc)
{
    uint32_t *new_flags = NULL;
    uint32_t j = 1;
    new_n_buckets = svm_hash__roundup(new_n_buckets);
    if (new_n_buckets < 4)
        new_n_buckets = 4;
    if (h->size >= (uint32_t)(new_n_buckets * SVM_HASH_UPPER + 0.5)) {
        j = 0;
    } else {
        size_t size = svm_hash__fsize(new_n_buckets) * sizeof(uint32_t);
        new_flags = svm_alloc(alloc, size);
        svm_assert_paranoid(new_flags);
        memset(new_flags, 0xaa, size);
        if (h->n_buckets < new_n_buckets) {
            h->keys = svm_realloc(alloc, h->keys, h->n_buckets * sizeof(key_t), new_n_buckets * sizeof(key_t));
            svm_assert_paranoid(h->keys);
            h->vals = svm_realloc(alloc, h->vals, h->n_buckets * sizeof(val_t), new_n_buckets * sizeof(val_t));
            svm_assert_paranoid(h->vals);
        }
    }

    if (j) {
        for (j = 0; j != h->n_buckets; j++)
        {
            if (svm_hash__iseither(h->flags, j) == 0) {
                key_t key = h->keys[j];
                val_t val = h->vals[j];
                uint32_t new_mask = new_n_buckets - 1;
                svm_hash__set_isdel_true(h->flags, j);
                for (;;)
                {
                    uint32_t k, i, step;
                    k = key_hash(key);
                    i = k & new_mask;
                    step = 0;

                    while (!svm_hash__isempty(new_flags, i))
                        i = (i + (++step)) & new_mask;
                    svm_hash__set_isempty_false(new_flags, i);
                    if (i < h->n_buckets && svm_hash__iseither(h->flags, i) == 0) {
                        {
                            key_t tmp = h->keys[i];
                            h->keys[i] = key;
                            key = tmp;
                        }
                        {
                            val_t tmp = h->vals[i];
                            h->vals[i] = val;
                            val = tmp;
                        }
                        svm_hash__set_isdel_true(h->flags, i);
                    } else {
                        h->keys[i] = key;
                        h->vals[i] = val;
                        break;
                    }
                }
            }
        }
        if (h->n_buckets > new_n_buckets) {
            h->keys = svm_realloc(alloc, h->keys, h->n_buckets * sizeof(key_t), new_n_buckets * sizeof(key_t));
            h->vals = svm_realloc(alloc, h->vals, h->n_buckets * sizeof(val_t), new_n_buckets * sizeof(val_t));
        }
        svm_free(alloc, h->flags, svm_hash__fsize(h->n_buckets) * sizeof(uint32_t));
        h->flags = new_flags;
        h->n_buckets = new_n_buckets;
        h->n_occupied = h->size;
        h->upper_bound = h->n_buckets * SVM_HASH_UPPER + 0.5;
    }
}

/* *ret is set to:
 *   0 if the key already exists
 *   1 if the key was not there
 *   2 if the slot was in a deleted state
 */
static inline uint32_t svm_hash_put(svm_hash_t *h, key_t key, int *ret, struct svm_allocator_t *alloc)
{
    uint32_t x;
    if (h->n_occupied >= h->upper_bound) {
        if (h->n_buckets > (h->size << 1)) {
            svm_hash_resize(h, h->n_buckets - 1, alloc);
        } else {
            svm_hash_resize(h, h->n_buckets + 1, alloc);
        }
    }
    {
        uint32_t k, i, site, last, mask, step;
        x = site = h->n_buckets;
        k = key_hash(key);
        mask = h->n_buckets - 1;
        step = 0;
        i = k & mask;

        if (svm_hash__isempty(h->flags, i)) {
            x = i;
        } else {
            last = i;
            while (!svm_hash__isempty(h->flags, i) && (svm_hash__isdel(h->flags, i) || !key_eq(h->keys[i], key))) {
                if (svm_hash__isdel(h->flags, i)) {
                    site = i;
                }
                i = (i + (++step)) & mask;
                if (i == last) {
                    x = site;
                    break;
                }
            }
            if (x == h->n_buckets) {
                if (svm_hash__isempty(h->flags, i) && site != h->n_buckets) {
                    x = site;
                } else {
                    x = i;
                }
            }
        }
    }
    if (svm_hash__isempty(h->flags, x)) {
        h->keys[x] = key;
        svm_hash__set_isboth_false(h->flags, x);
        h->size++;
        h->n_occupied++;
        if (ret)
            *ret = 1;
    } else if (svm_hash__isdel(h->flags, x)) {
        h->keys[x] = key;
        svm_hash__set_isboth_false(h->flags, x);
        h->size++;
        if (ret)
            *ret = 2;
    } else {
        if (ret)
            *ret = 0;
    }
    return x;
}

static void svm_hash_del(svm_hash_t *h, uint32_t x)
{
    if (x != h->n_buckets && !svm_hash__iseither(h->flags, x)) {
        svm_hash__set_isdel_true(h->flags, x);
        h->size--;
    }
}

#endif /* ifndef SVM_HASH_H */
