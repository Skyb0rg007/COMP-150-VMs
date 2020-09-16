
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>
#include <tmmintrin.h> /* SSSE3 */
#include "simd_vtable.h"

#define kEmpty    ((int8_t)(-128))
#define kDeleted  ((int8_t)(-2))
#define kSentinel ((int8_t)(-1))

#define GROUP_WIDTH 16

#define MAX(a, b) ((a) > (b) ? (a) : (b))

_Static_assert(sizeof(unsigned long long) == sizeof(uint64_t), "__builtin_ctzll() doesn't take uint64_t");
_Static_assert(sizeof(unsigned int) == sizeof(uint32_t), "__builtin_ctz() doesn't take uint32_t");

static inline int trailing_zeros64(uint64_t n) { return __builtin_ctzll(n); }
static inline int trailing_zeros32(uint32_t n) { return __builtin_ctz(n); }
static inline int leading_zeros64(uint64_t n) { return __builtin_clzll(n); }
static inline int leading_zeros32(uint32_t n) { return __builtin_clz(n); }

/* The H1 hash - determines which group to look at */
static inline uint64_t H1(uint64_t hash) {
    return hash >> 7;
}

/* The H2 hash - this is stuffed into the control byte */
static inline int8_t H2(uint64_t hash) {
    return hash & 0x7f;
}

/* Returns a bitmask representing the positions of slots that match hash */
__attribute__((const))
static int match(__m128i ctrl, uint8_t hash) {
    __m128i match = _mm_set1_epi8(hash);
    return _mm_movemask_epi8(_mm_cmpeq_epi8(match, ctrl));
}

/* Returns a bitmask representing the positions of empty slots */
__attribute__((const))
static int match_empty(__m128i ctrl) {
    return _mm_movemask_epi8(_mm_sign_epi8(ctrl, ctrl));
}

/* Returns a bitmask representing the positions of empty or deleted slots */
__attribute__((const))
static int match_empty_or_deleted(__m128i ctrl) {
    const __m128i special = _mm_set1_epi8(kSentinel);
    return _mm_movemask_epi8(_mm_cmpgt_epi8(special, ctrl));
}

/* Returns the number of trailing empty or deleted elements in the group */
__attribute__((const))
static uint32_t count_leading_empty_or_deleted(__m128i ctrl) {
    const __m128i special = _mm_set1_epi8(kSentinel);
    return trailing_zeros32(_mm_movemask_epi8(_mm_cmpgt_epi8(special, ctrl)) + 1);
}

__attribute__((const))
static __m128i convert_special_to_empty_and_full_to_deleted(__m128i ctrl) {
    __m128i msbs = _mm_set1_epi8(kEmpty /* -128 */);
    __m128i x126 = _mm_set1_epi8(126);
    return _mm_or_si128(_mm_shuffle_epi8(x126, ctrl), msbs);
}

/* Determine if a capacity is valid, i.e. a nonzero power of 2 */
__attribute__((const))
static inline bool isValidCapacity(uint64_t c) {
    return ((c + 1) & c) == 0 && c > 0;
}

/* Rounds up the capacity to the next power of 2 minus 1, with a minimum of 1 */
__attribute__((const))
static inline uint64_t normalize_capacity(uint64_t n) {
    return n ? ~((uint64_t)0) >> leading_zeros64(n) : 1;
}

/* Use a 7/8 max load factor */
__attribute__((const))
static inline uint64_t capacity_to_growth(uint64_t capacity) {
    assert(isValidCapacity(capacity));
    return capacity - capacity / 8;
}

/* From desired growth to a lower bound of needed capacity
 * May not be valid - use normalize_capacity on the result
 */
__attribute__((const))
static inline uint64_t growth_to_lower_bound_capacity(uint64_t growth) {
    return growth + (uint64_t)(((int64_t)growth - 1) / 7);
}

/* Returns q*m for the smallest q such that q*m >= n
 * m must be a power of 2
 */
__attribute__((const))
static inline size_t align(size_t n, size_t m) {
    assert((m != 0) && (m & (m - 1)) == 0);
    return (n + m - 1) & ~(m - 1);
}

static void reset_ctrl(VTable_T t) {
    memset(t->ctrl_, kEmpty, t->capacity_ + GROUP_WIDTH);
    t->ctrl_[t->capacity_] = kSentinel;
}

static void reset_growth_left(VTable_T t) {
    t->growth_left_ = capacity_to_growth(VTable_capacity(t)) - t->size_;
}

/* Initialize slots - overwrites ctrl_ and slots_ */
static void initialize_slots(VTable_T t) {
    assert(t->capacity_);
    size_t alignment = MAX(_Alignof(int8_t), _Alignof(struct VTable_KV_T));
    /* One control byte per elem, plus additional for scanning, and one for sentinel */
    size_t ctrl_size = t->capacity_ + GROUP_WIDTH + 1 * sizeof(int8_t);
    size_t ctrl_mem_size = align(ctrl_size, _Alignof(struct VTable_KV_T));
    size_t slot_size = t->capacity_ * sizeof(struct VTable_KV_T);
    char *mem = aligned_alloc(alignment, ctrl_mem_size + slot_size);
    assert(mem != NULL);
    t->ctrl_ = (int8_t *)mem;
    t->slots_ = (struct VTable_KV_T *)(mem + ctrl_mem_size);
    reset_ctrl(t);
    reset_growth_left(t);
}

static int8_t *EmptyGroup(void) {
    _Alignas(16) static const int8_t empty_group[16] = {
        kSentinel, kEmpty, kEmpty, kEmpty, kEmpty, kEmpty, kEmpty, kEmpty,
        kEmpty,    kEmpty, kEmpty, kEmpty, kEmpty, kEmpty, kEmpty, kEmpty
    };
    return (int8_t *)empty_group;
}

static void destroy_slots(VTable_T t) {
    if (!t->capacity_)
        return;
    free(t->ctrl_);
    t->ctrl_ = EmptyGroup();
    t->slots_ = NULL;
    t->size_ = 0;
    t->capacity_ = 0;
    t->growth_left_ = 0;
}

struct probe_seq {
    size_t mask_, offset_, index_;
};

static size_t probe_seq_offset(const struct probe_seq *seq, size_t i) {
    return (seq->offset_ + i) & seq->mask_;
}

static void probe_seq_next(struct probe_seq *seq) {
    seq->index_ += GROUP_WIDTH;
    seq->offset_ += seq->index_;
    seq->offset_ &= seq->mask_;
}

static struct probe_seq probe(VTable_T t, uint64_t hash) {
    return (struct probe_seq){
        .mask_ = t->capacity_,
        .offset_ = hash & t->capacity_,
        .index_ = 0
    };
}

static size_t find_first_non_full(VTable_T t, uint64_t hash) {
    struct probe_seq seq = probe(t, hash);
    while (true) {
        size_t offset = seq.offset_;
        int mask = match_empty_or_deleted(*(__m128i *)(t->ctrl_ + offset));
        if (mask) {
            return probe_seq_offset(&seq, sizeof(mask) * CHAR_BIT - leading_zeros32(mask) - 1);
        }
    }
}

static void resize(VTable_T t, size_t new_capacity) {
    assert(isValidCapacity(new_capacity));
    int8_t *old_ctrl = t->ctrl_;
    struct VTable_KV_T *old_slots = t->slots_;
    size_t old_capacity = t->capacity_;
    t->capacity_ = new_capacity;
    initialize_slots(t);

    for (size_t i = 0; i != old_capacity; i++) {
        if (IsFull(old_ctrl[i])) {
            uint64_t hash = hashvalue(old_slots[i].key);
            size_t new_i = find_first_non_full(t, hash);
            set_ctrl(t, new_i, H2(hash));
            t->slots_[new_i] = old_slots[i];
        }
    }
}

extern size_t VTable_capacity(const VTable_T t)
{
    return t->capacity_;
}

/* static inline void resize(VTable_T t, size_t new_capacity) */
/* { */
    /* assert(isValidCapacity(new_capacity)); */
    /* __m128i *old_ctrl = t->ctrl; */
    /* struct VTable_KV_T *old_slots = t->slots; */
    /* size_t old_cap = t->capacity; */
    /* t->capacity = new_capacity; */
/* } */

/* static inline void VTable_init(VTable_T *t)
{
    static const int HINT = 1; [>Number of groups to allocate<]
    char *mem;
    int i, j;

    mem = vmalloc(
            HINT * sizeof(t->ctrl[0]) +
            HINT * 16 * sizeof(t->slots[0]));
    assert(mem != NULL);
    t->ctrl = (__m128i *)mem;
    t->slots = (VTable_KV_T *)(mem + sizeof(__m128i) * HINT);
    t->size = 0;
    t->capacity = HINT;
    for (i = 0; i < HINT; i++) {
        t->ctrl[i] = _mm_set1_epi8(VTABLE_K_EMPTY);
        for (j = 0; j < 16; j++) {
            t->slots[16 * i + j].key = nilValue;
            t->slots[16 * i + j].value = nilValue;
        }
    }
}

static inline Value VTable_put(VTable_T *t, Value key, Value value)
{
    uint64_t hash = hashvalue(key);
    size_t offset;
    { [>Find first non-full<]
        size_t group = VTable_H1(hash) & t->capacity;
        while (true) {
            int m = VTable_match_empty_or_deleted(t->ctrl[group]);
            if (m) {
                offset = group * 16 + __builtin_ctz(m);
                break;
            }
            group = (group + 1) & t->capacity;
        }
    }
    return nilValue;
} */

