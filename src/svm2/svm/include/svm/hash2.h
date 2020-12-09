/** @file svm/hash.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Hashtables and hashing functions
 *
 * This implementation comes from khash.h in klib
 */
#ifndef SVM_HASHTABLE_H
#define SVM_HASHTABLE_H

#include <svm/config.h>
#include <svm/value.h>

struct svm_hashtable_t;

static inline void svm_hashtable_init(struct svm_hashtable_t *h);
extern void svm_hashtable_gc(struct svm_hashtable_t *h);
static inline void svm_hashtable_free(struct svm_hashtable_t *h, struct svm_allocator_t *alloc);
extern struct svm_value_t *svm_hashtable_get(struct svm_hashtable_t *h, struct svm_value_t *k) SVM_ATTR_NONNULL(1, 2);
extern void svm_hashtable_put(struct svm_hashtable_t *h, struct svm_value_t *k, struct svm_value_t *v, struct svm_allocator_t *alloc) SVM_ATTR_NONNULL(1, 2, 3);
/* svm_hashtable_foreach(h, k, v) */

struct svm_hashtable_t {
    uint32_t n_buckets, size, n_occupied, upper_bound;
    uint32_t *flags;
    struct svm_value_t *keys;
    struct svm_value_t *vals;
};
static inline void svm_hashtable_init(struct svm_hashtable_t *h)
{
    h->n_buckets = h->size = h->n_occupied = h->upper_bound = 0;
    h->flags = NULL;
    h->keys = NULL;
    h->vals = NULL;
}

#define SVM_HASHTABLE_UPPER 0.77

/* static inline bool svm_hash__isempty(uint32_t *flags, uint32_t i); */
/* static inline bool svm_hash__isdel(uint32_t *flags, uint32_t i); */
/* static inline bool svm_hash__iseither(uint32_t *flags, uint32_t i); */
/* static inline void svm_has__set_isdel_false(uint32_t *flags, uint32_t i); */
/* static inline void svm_hash__set_isempty_false(uint32_t *flags, uint32_t i); */
/* static inline void svm_hash__set_isboth_false(uint32_t *flags, uint32_t i); */
/* static inline void svm_hash__set_isdel_true(uint32_t *flags, uint32_t i); */
/* static inline size_t svm_hash__fsize(uint32_t m); */
/* static inline uint32_t svm_hash__roundup(uint32_t x); */

/* Hashtables */

struct svm_hashtable_t {
    uint32_t n_buckets, size, n_occupied, upper_bound;
    uint32_t *flags;
    struct svm_value_t *keys;
    struct svm_value_t *vals;
};

static inline void svm_hashtable_init(struct svm_hashtable_t *h)
{
    h->n_buckets = h->size = h->n_occupied = h->upper_bound = 0;
    h->flags = NULL;
    h->keys = NULL;
    h->vals = NULL;
}
static inline bool svm_hashtable_exists(struct svm_hashtable_t *h, uint32_t it) {
    return !svm_hash__iseither(h->flags, it);
}
static inline size_t svm_hashtable_size(struct svm_hashtable_t *h) {
    return h->size;
}
static inline size_t svm_hashtable_n_buckets(struct svm_hashtable_t *h) {
    return h->n_buckets;
}
static inline uint32_t svm_hashtable_begin(struct svm_hashtable_t *h) {
    SVM_UNUSED(h);
    return 0;
}
static inline uint32_t svm_hashtable_end(struct svm_hashtable_t *h) {
    return svm_hashtable_n_buckets(h);
}
#define svm_hashtable_key(h, it) ((h)->keys[it])
#define svm_hashtable_val(h, it) ((h)->vals[it])
#define svm_hashtable_foreach(it, h)                                          \
    for ((it) = svm_hashtable_begin(h); (it) != svm_hashtable_end(h); (it)++) \
    if (!svm_hashtable_exists(h, it))                                     \
    continue;                                                         \
    else

/* String set */

struct svm_stringset_t {
    uint32_t n_buckets, size, n_occupied, upper_bound;
    uint32_t *flags;
    struct svm_symbol_t *keys;
};

static inline void svm_stringset_init(struct svm_stringset_t *h)
{
    h->n_buckets = h->size = h->n_occupied = h->upper_bound = 0;
    h->flags = NULL;
    h->keys = NULL;
}
static inline bool svm_stringset_exists(struct svm_stringset_t *h, uint32_t it) {
    return !svm_hash__iseither(h->flags, it);
}
static inline size_t svm_stringset_size(struct svm_stringset_t *h) {
    return h->size;
}
static inline size_t svm_stringset_n_buckets(struct svm_stringset_t *h) {
    return h->n_buckets;
}
static inline uint32_t svm_stringset_begin(struct svm_stringset_t *h) {
    SVM_UNUSED(h);
    return 0;
}
static inline uint32_t svm_stringset_end(struct svm_stringset_t *h) {
    return svm_stringset_n_buckets(h);
}
#define svm_stringset_key(h, it) ((h)->keys[it])
#define svm_stringset_foreach(it, h)                                          \
    for ((it) = svm_stringset_begin(h); (it) != svm_stringset_end(h); (it)++) \
    if (!svm_stringset_exists(h, it))                                     \
    continue;                                                         \
    else

extern uint32_t svm_hashtable_get(struct svm_hashtable_t *h, struct svm_value_t *val);
extern void svm_hashtable_resize(struct svm_hashtable_t *h, uint32_t new_n_buckets, struct svm_allocator_t *alloc);

extern uint32_t svm_stringset_get(struct svm_stringset_t *h, uint32_t hash, const char *key, size_t key_len);
extern void svm_stringset_resize(struct svm_stringset_t *h, uint32_t new_n_buckets, struct svm_allocator_t *alloc);

extern uint32_t svm_hashtable_put(struct svm_hashtable_t *h, uint32_t hash, const char *key, size_t key_len);
extern uint32_t svm_stringset_put(struct svm_stringset_t *h, uint32_t hash, const char *key, size_t key_len);


/* #define svm_hashtable_t(key_t, val_t)                      \
   struct {                                               \
   uint32_t n_buckets, size, n_occupied, upper_bound; \
   uint32_t *flags;                                   \
   key_t *keys;                                       \
   val_t *vals;                                       \
   }


#define svm_hashtable_free(h, alloc) do {    \
if (h) {                            \
svm_free(alloc, (h)->keys, 0);  \
svm_free(alloc, (h)->flags, 0); \
svm_free(alloc, (h)->vals, 0);  \
}                                   \
} while (0)

#define svm_hashtable_clear(h) do {                                     \
if ((h) && (h)->flags) {                                       \
memset((h)->flags, 0xaa, svm_hash__fsize((h)->n_buckets)); \
(h)->size = (h)->n_occupied = 0;                           \
}                                                              \
} while (0)

#define svm_hashtable_del(h, it) do {                                      \
if (it != svm_hashtable_end(h) && !svm_hash__iseither((h)->flags, it)) { \
svm_hash__set_isdel_true((h)->flags, it);                      \
(h)->size--;                                                  \
}                                                                 \
} while (0)

typedef svm_hashtable_t(struct svm_value_t, struct svm_value_t) svm_table_t;

#define svm_hashtable_DEFINE(type, key_t, val_t, hash_key, keys_eq)

#define type svm_hashtable_t(int, int)
#define key_t int
#define val_t int
#define hash_key(x) (x)
#define keys_eq(a, b) ((a) == (b))

static inline int svm_hashtable_get(type *h, key_t key)
{
if (!h->n_buckets) {
return 0;
}

uint32_t k, i, last, mask, step;
step = 0;
mask = h->n_buckets - 1;
k = hash_key(key);
last = i = k & mask;
while (!svm_hash__isempty(h->flags, i) && (svm_hash__isdel(h->flags, i) || !keys_eq(h->keys[i], key)))
{
i = (i + (++step)) & mask;
if (i == last)
return h->n_buckets;
}
return svm_hash__iseither(h->flags, i)
? h->n_buckets
: i;
}

static inline void svm_hashtable_resize(struct svm_hashtable_t *h, uint32_t new_n_buckets, struct svm_allocator_t *alloc)
{
uint32_t *new_flags = NULL;
uint32_t j = 1;
new_n_buckets = svm_hash__roundup(new_n_buckets);
if (new_n_buckets < 4)
new_n_buckets = 4;
if (h->size >= (uint32_t)(new_n_buckets * svm_hashtable_UPPER + 0.5)) {
j = 0;
} else {
    size_t size = svm_hash__fsize(new_n_buckets) * sizeof(uint32_t);
    new_flags = svm_alloc(alloc, size);
    svm_assert_paranoid(new_flags);
    memset(new_flags, 0xaa, size);
    if (h->n_buckets < new_n_buckets) {
        h->keys = svm_realloc(alloc, h->keys, h->n_buckets * sizeof(struct svm_value_t), new_n_buckets * sizeof(struct svm_value_t));
        svm_assert_paranoid(h->keys);
        h->vals = svm_realloc(alloc, h->vals, h->n_buckets * sizeof(struct svm_value_t), new_n_buckets * sizeof(struct svm_value_t));
        svm_assert_paranoid(h->vals);
    }
}

if (j) {
    for (j = 0; j != h->n_buckets; j++)
    {
        if (svm_hash__iseither(h->flags, j) == 0) {
            struct svm_value_t key = h->keys[j];
            struct svm_value_t val = h->vals[j];
            uint32_t new_mask = new_n_buckets - 1;
            svm_hash__set_isdel_true(h->flags, j);
            for (;;)
            {
                uint32_t k, i, step;
                k = svm_value_hash(&key);
                i = k & new_mask;
                step = 0;

                while (!svm_hash__isempty(new_flags, i))
                    i = (i + (++step)) & new_mask;
                svm_hash__set_isempty_false(new_flags, i);
                if (i < h->n_buckets && svm_hash__iseither(h->flags, i) == 0) {
                    {
                        struct svm_value_t tmp = h->keys[i];
                        h->keys[i] = key;
                        key = tmp;
                    }
                    {
                        struct svm_value_t tmp = h->vals[i];
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
        h->keys = svm_realloc(alloc, h->keys, h->n_buckets * sizeof(struct svm_value_t), new_n_buckets * sizeof(struct svm_value_t));
        h->vals = svm_realloc(alloc, h->vals, h->n_buckets * sizeof(struct svm_value_t), new_n_buckets * sizeof(struct svm_value_t));
    }
    svm_free(alloc, h->flags, svm_hash__fsize(h->n_buckets) * sizeof(uint32_t));
    h->flags = new_flags;
    h->n_buckets = new_n_buckets;
    h->n_occupied = h->size;
    h->upper_bound = h->n_buckets * svm_hashtable_UPPER + 0.5;
}
}

[>*ret is set to:
*   0 if the key already exists
*   1 if the key was not there
*   2 if the slot was in a deleted state
<]
static inline uint32_t svm_hashtable_put(struct svm_hashtable_t *h, struct svm_value_t key, int *ret, struct svm_allocator_t *alloc)
{
    uint32_t x;
    if (h->n_occupied >= h->upper_bound) {
        if (h->n_buckets > (h->size << 1)) {
            svm_hashtable_resize(h, h->n_buckets - 1, alloc);
        } else {
            svm_hashtable_resize(h, h->n_buckets + 1, alloc);
        }
    }
    {
        uint32_t k, i, site, last, mask, step;
        x = site = h->n_buckets;
        k = svm_value_hash(&key);
        mask = h->n_buckets - 1;
        step = 0;
        i = k & mask;

        if (svm_hash__isempty(h->flags, i)) {
            x = i;
        } else {
            last = i;
            while (!svm_hash__isempty(h->flags, i) && (svm_hash__isdel(h->flags, i) || !svm_value_eq(&h->keys[i], &key))) {
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
} */

/*****************************************************************************
 * Private
 ****************************************************************************/

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


#endif /* ifndef SVM_HASHTABLE_H */
