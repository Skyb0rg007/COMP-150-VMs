/** @file svm/vector.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Polymorphic vectors in pure C
 *
 * The concrete representation is lifted from C++ std::vector implementations.
 * While most C vectors store size & capacity as integers, the common idiom
 * of iterating over a C++ vector means that begin() and end() should be stored instead.
 * I opt for this modified approach, since it simplifies iteration to only need one variable.
 * Since it's used in C++ I assume that it doesn't cause performance issues for
 * resizing or querying vector size.
 *
 * This module is also specialized for using the svm_allocator_t struct.
 * All memory allocations for the svm use this struct, so we cannot just use realloc + free.
 *
 * This vector can only handle relocatable objects. By this I mean objects that
 * can be freely memcopyed from one memory buffer to another without any problems.
 * Any object that is pointed to cannot be stored in this vector.
 * Some objects may also point to themselves (such as in the Linux doubly-linked list implementation).
 * This doesn't work here either.
 *
 * Basic assertions are enabled for debug level 1 or higher.
 * Assertions such as ensuring the vector is not NULL are not done.
 * Reallocation is assumed to return a valid pointer.
 */
#ifndef SVM_VECTOR_H
#define SVM_VECTOR_H

#include <svm/config.h>
#include <svm/assert.h>
#include <svm/heap.h>
#include <svm/vm.h>

/** @brief The polymorphic vector type
 * @param T The type the vector holds
 * @note The vector cannot handle non-relocatable objects
 * @note @p T must not be able to go before the variable name. No arrays or function types allowed, unless typedefed.
 * @hideinitializer
 */
#define svm_vector_t(T) \
    struct { T *__begin_, *__end_, *__end_cap_; }

/*****************************************************************************
 *
 * Non-allocating routines
 *
 ****************************************************************************/

/** @brief Initialize a vector
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_init(v) \
    ((v)->__begin_ = (v)->__end_ = (v)->__end_cap_ = NULL)

/** @brief Get the start iterator
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_begin(v) \
    ((v)->__begin_)

/** @brief Get the end iterator
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_end(v) \
    ((v)->__end_)

/** @brief Get the start reverse iterator
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_rbegin(v) \
    ((v)->__end_ - 1)

/** @brief Get the end reverse iterator
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_rend(v) \
    ((v)->__begin_ - 1)

/** @brief Get the current size of the vector
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_size(v) \
    ((size_t)((v)->__end_ - (v)->__begin_))

/** @brief Query the maximum number of elements the vector can allocate
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_max_size(v) \
    ((void)(v), PTRDIFF_MAX / sizeof(*(v)->__begin_))

/** @brief Get the current capacity of the vector
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_capacity(v) \
    ((size_t)((v)->__end_cap_ - (v)->__begin_))

/** @brief Determine if the vector is empty
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_empty(v) \
    ((bool)((v)->__begin_ == (v)->__end_))

/** @brief Access a vector element
 * @param v svm_vector_t(T) *
 * @param n size_t
 * @return A modifiable lvalue of the vector element at index @p n
 * @hideinitializer
 */
#define svm_vector_at(v, n) \
    (*(svm_assert((n) >= svm_vector_size(v)), (v)->__begin_ + (n)))

/** @brief Access the first vector element
 * @param v svm_vector_t(T) *
 * @return A modifiable lvalue of the vector element at index 0
 * @hideinitializer
 */
#define svm_vector_front(v) \
    (*(svm_assert(!svm_vector_empty(v)), (v)->__begin_))

/** @brief Access the last vector element
 * @param v svm_vector_t(T) *
 * @return A modifiable lvalue of the vector element at the last index
 * @hideinitializer
 */
#define svm_vector_back(v) \
    (*(svm_assert(!svm_vector_empty(v)), (v)->__end_ - 1))

/** @brief Access the vector's internal storage
 * @param v svm_vector_t(T) *
 * @return T *
 * @hideinitializer
 */
#define svm_vector_data(v) \
    ((v)->__begin_)

/** @brief Pop an element from the end
 * @param v svm_vector_t(T) *
 * @hideinitializer
 */
#define svm_vector_pop_back(v) \
    (svm_assert(!svm_vector_empty(v)), (v)->__end_--, (void)0)

/*****************************************************************************
 *
 * Allocating routines
 *
 ****************************************************************************/

/** @brief Free all allocated memory
 * @param v svm_vector_t(T) *
 * @param a struct svm_allocator_t *
 * @hideinitializer
 */
#define svm_vector_free(v, a) \
    ((void)((a)->fun((v)->__begin_, (char *)(v)->__begin_ - (char *)(v)->__end_cap_, 0, (a)->ud)))

/** @brief Ensure that the vector's capacity is at least @p n
 * @param v svm_vector_t(T) *
 * @param n size_t
 * @param a struct svm_allocator_t *
 * @hideinitializer
 */
#define svm_vector_reserve(v, n, a) \
    ((n) <= svm_vector_capacity(v)  \
     ? (void)0                      \
     : svm_vector__setcap(&(v)->__begin_, &(v)->__end_, &(v)->__end_cap_, sizeof(*(v)->__begin_), n, a))

/** @brief Reduce vector's capacity to its size
 * @param v svm_vector_t(T) *
 * @param a struct svm_allocator_t *
 * @hideinitializer
 */
#define svm_vector_shrink_to_fit(v, a)            \
    (svm_vector_capacity(v) <= svm_vector_size(v) \
     ? (void)0                                    \
     : svm_vector__setcap(&(v)->__begin_, &(v)->__end_, &(v)->__end_cap_, sizeof(*(v)->__begin_), svm_vector_size(v), a))

/** @brief Insert an element into the vector at the end
 * @param v svm_vector_t(T) *
 * @param x T
 * @param a struct svm_allocator_t *
 * @hideinitializer
 */
#define svm_vector_push_back(v, x, a) \
    (*svm_vector_emplace_back(v, a) = (x), (void)0)

/** @brief Reserve space at the end of the vector, returning a pointer to it
 * @param v svm_vector_t(T) *
 * @param a struct svm_allocator_t *
 * @hideinitializer
 */
#define svm_vector_emplace_back(v, a)                                                \
    (((v)->__end_ != (v)->__end_cap_                                                 \
      ? (void)0                                                                      \
      : svm_vector_reserve(v, svm_vector__recommend(v, svm_vector_size(v) + 1), a)), \
     (v)->__end_++)

/* TODO:
 * emplace
 * insert
 * erase
 * resize
 */

/** @brief Clear the vector
 * @param v svm_vector_t(T) *
 * @param a struct svm_allocator_t *
 * @hideinitializer
 */
#define svm_vector_clear(v) \
    (svm_vector_free(v), svm_vector_init(v))

/* Internal macro
 * Given a size, recommend a capacity larger than that size
 */
#define svm_vector__recommend(v, n)                                 \
    (svm_assert((n) <= svm_vector_max_size(v)),                     \
     svm_vector_capacity(v) >= svm_vector_max_size(v) / 2           \
     ? svm_vector_max_size(v)                                       \
     : SVM_MAX(2 * svm_vector_capacity(v), n))

/* Internal function
 * Sets the capacity of the vector to n
 * Takes the struct members as pointers. 'void *' is used instead of 'void **' to prevent warnings.
 */
SVM_ATTR_NONNULL(1, 2, 3, 6)
static inline void svm_vector__setcap(void *_begin, void *_end, void *_end_cap, size_t elem_size, size_t n, struct svm_allocator_t *a)
{
    char **begin = _begin;
    char **end = _end;
    char **end_cap = _end_cap;

    size_t end_offset = *end - *begin;
    size_t end_cap_offset = *end_cap - *begin;
    *begin = a->fun(*begin, end_cap_offset, elem_size * n, a->ud);
    svm_assert_paranoid(*begin != NULL);
    *end = *begin + end_offset;
    *end_cap = *begin + elem_size * n;
}

#endif /* ifndef SVM_VECTOR_H */
