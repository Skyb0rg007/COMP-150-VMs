#ifndef SIMD_VTABLE_H
#define SIMD_VTABLE_H

#include <stddef.h>
#include "../value.h"
#include "../vmheap.h"

struct VTable_KV_T {
    Value key;
    Value value;
};

typedef struct VTable_T *VTable_T;
struct VTable_T {
    int8_t *ctrl_;              /* Control bytes - 8 bits per key, in 128-bit groups */
    struct VTable_KV_T *slots_; /* Key-Value elements */
    uint64_t size_,             /* Number of elements */
             capacity_,         /* Capacity, always a bitmask */
             growth_left_;      /* Growth left */
};

extern void VTable_init(VTable_T t);
extern Value VTable_put(VTable_T t, Value key, Value value);
extern Value VTable_get(VTable_T t, Value key);
extern Value VTable_remove(VTable_T t, Value key);
extern size_t VTable_capacity(const VTable_T t);

#endif /* ifndef SIMD_VTABLE_H */
