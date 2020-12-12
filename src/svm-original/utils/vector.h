/* Generic dynamic array implementation using malloc/free */
#ifndef UTILS_VECTOR_H
#define UTILS_VECTOR_H

#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>

/* The vector type */
#define vector(type) struct { type *_begin, *_end, *_end_cap; }

#define vector_init(v) \
    ((void)((v)->_begin = (v)->_end = (v)->_end_cap = NULL))

#define vector_free(v) \
    free((v)->_begin)

#define vector_capacity(v) \
    ((size_t)((v)->_end_cap - (v)->_begin))

#define vector_size(v) \
    ((size_t)((v)->_end - (v)->_begin))

#define vector_at(v, idx) \
    ((v)->_begin[(idx)])

#define vector_reserve(v, n)                                       \
    ((n) > am_vector_capacity(v)                                   \
     ? vector__setcap((vector__void *)v, sizeof(*(v)->_begin) * n) \
     : true)

#define vector_emplace_back(v)                                                                        \
    ((vector_size(v) == vector_capacity(v)                                                            \
      ? vector__setcap((vector__void *)v, sizeof(*(v)->_begin) * vector__nextcap(vector_capacity(v))) \
      : true)                                                                                         \
        ? (v)->_end++                                                                                 \
        : NULL)

#define vector_foreach(it, v) \
    for ((it) = (v)->_begin; (it) != (v)->_end; (it)++)

typedef vector(void) vector__void;

static inline bool vector__setcap(vector__void *v, size_t new_cap)
{
    void *newbegin;
    size_t size_total = (char *)v->_end - (char *)v->_begin;
    newbegin = realloc(v->_begin, new_cap);
    if (!newbegin)
        return false;
    v->_begin = newbegin;
    v->_end = (char *)newbegin + size_total;
    v->_end_cap = (char *)newbegin + new_cap;
    return true;
}

static inline size_t vector__nextcap(size_t cap)
{
    return cap < 2 ? 2 : cap * 2;
}

#endif /* ifndef UTILS_VECTOR_H */
