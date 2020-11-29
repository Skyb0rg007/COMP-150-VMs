/** @file svm/value.h
 * @author Skye Soss
 * @copyright MIT
 * @brief Internal representation of svm values
 */
#ifndef SVM_VALUE_H
#define SVM_VALUE_H

#include <svm/config.h>
#include <svm/instruction.h>
#include <svm/assert.h>

/** @brief The SVM value tags */
enum svm_value_tag_t {
    SVM_VALUE_TAG_VOID,      /* #<void> */
    SVM_VALUE_TAG_BOOLEAN,   /* #t #f */
    SVM_VALUE_TAG_CHAR,      /* #\a #\b */
    SVM_VALUE_TAG_NUMBER,    /* 3.14 #xdeadbeef */
    SVM_VALUE_TAG_STRING,    /* "foo" "bar" */
    SVM_VALUE_TAG_SYMBOL,    /* 'foo '|bar| */
    SVM_VALUE_TAG_EMPTYLIST, /* '() */
    SVM_VALUE_TAG_CONS,      /* (cons 1 2) */
    SVM_VALUE_TAG_FUNCTION,
    SVM_VALUE_TAG_CLOSURE,
    SVM_VALUE_TAG_BLOCK,
    SVM_VALUE_TAG_TABLE,
    SVM_VALUE_TAG_CONTINUATION
};

/** @brief Returns a string representation of a given value tag
 * @param tag The tag to query
 * @return A statically-allocated string describing the tag
 */
extern const char *svm_value_tag_name(enum svm_value_tag_t tag)
    SVM_ATTR_RETURNS_NONNULL;

/** @brief Representation of an SVM value (tagged union) */
struct svm_value_t {
    enum svm_value_tag_t tag;
    union {
        bool as_boolean;
        int as_char;
        double as_number;
        struct svm_string_t *as_string;
        struct svm_string_t *as_symbol;
        struct svm_block_t *as_cons;
        struct svm_function_t *as_function;
        struct svm_closure_t *as_closure;
        struct svm_block_t *as_block;
        struct svm_table_t *as_table;
        struct svm_activation_t *as_continuation;
    } rep;
};

extern void svm_value_print(struct svm_value_t *val, FILE *outfile);

/* Helpers for getting + setting values
 * I'm abstracting this now, since I may change the value representation later
 */
#define SVM_GETTER_SETTER(lower, upper, type)                                     \
    SVM_ATTR_NONNULL(1)                                                           \
    static inline void svm_value_set_##lower(struct svm_value_t *x, type y) {     \
        x->tag = SVM_VALUE_TAG_##upper;                                           \
        x->rep.as_##lower = y;                                                    \
    }                                                                             \
    SVM_ATTR_NONNULL(1)                                                           \
    static inline type svm_value_get_##lower(struct svm_value_t *x) {             \
        if (x->tag != SVM_VALUE_TAG_##upper)                                      \
            svm_panic("Expected " #lower ", got %s", svm_value_tag_name(x->tag)); \
        return x->rep.as_##lower;                                                 \
    }
#define SVM_GETTER_SETTER0(lower, upper)                                          \
    SVM_ATTR_NONNULL(1)                                                           \
    static inline void svm_value_set_##lower(struct svm_value_t *x) {             \
        x->tag = SVM_VALUE_TAG_##upper;                                           \
    }                                                                             \
    SVM_ATTR_NONNULL(1)                                                           \
    static inline void svm_value_get_##lower(struct svm_value_t *x) {             \
        if (x->tag != SVM_VALUE_TAG_##upper)                                      \
            svm_panic("Expected " #lower ", got %s", svm_value_tag_name(x->tag)); \
    }
SVM_GETTER_SETTER(boolean, BOOLEAN, bool)
SVM_GETTER_SETTER(char, CHAR, char)
SVM_GETTER_SETTER(number, NUMBER, double)
SVM_GETTER_SETTER(string, STRING, struct svm_string_t *)
SVM_GETTER_SETTER(symbol, SYMBOL, struct svm_string_t *)
SVM_GETTER_SETTER(cons, CONS, struct svm_block_t *)
SVM_GETTER_SETTER(function, FUNCTION, struct svm_function_t *)
SVM_GETTER_SETTER(closure, CLOSURE, struct svm_closure_t *)
SVM_GETTER_SETTER(block, BLOCK, struct svm_block_t *)
SVM_GETTER_SETTER(table, TABLE, struct svm_table_t *)
SVM_GETTER_SETTER(continuation, CONTINUATION, struct svm_activation_t *)
SVM_GETTER_SETTER0(void, VOID)
SVM_GETTER_SETTER0(emptylist, EMPTYLIST)

#undef SVM_GETTER_SETTER
#undef SVM_GETTER_SETTER0

/** @brief Returns true if the value is truthy, false otherwise */
static inline bool svm_value_truthy(struct svm_value_t *x)
{
    return x->tag != SVM_VALUE_TAG_BOOLEAN || x->rep.as_boolean;
}

/****************************************************************************
 *
 * Variant implementations
 *
 ****************************************************************************/

#define SVM_GC_META(t) struct t *forwarded

/** Used for strings and symbols */
struct svm_string_t {
    SVM_GC_META(svm_string_t);
    size_t length;
    uint32_t hash;
    struct svm_string_t *next_interned;
    char bytes[];
};

/** Used for cons and block */
struct svm_block_t {
    SVM_GC_META(svm_block_t);
    int nslots;
    struct svm_value_t slots[];
};

/** Used for functions */
struct svm_function_t {
    SVM_GC_META(svm_function_t);
    int arity;
    int size;
    int nregs;
    svm_instruction_t instructions[];
};

/** Closures - a combo of function and closed-over values */
struct svm_closure_t {
    SVM_GC_META(svm_closure_t);
    struct svm_function_t *fun;
    int nslots;
    struct svm_value_t slots[];
};

/* Helpers for determining allocation size */
static inline size_t svm_string_allocsize(size_t len)
{
    return sizeof(struct svm_string_t) + (len + 1) * sizeof(char);
}
static inline size_t svm_block_allocsize(size_t nslots)
{
    return sizeof(struct svm_block_t) + nslots * sizeof(struct svm_value_t);
}
static inline size_t svm_function_allocsize(size_t size)
{
    return sizeof(struct svm_function_t) + size * sizeof(svm_instruction_t);
}
static inline size_t svm_closure_allocsize(size_t nslots)
{
    return sizeof(struct svm_closure_t) + nslots * sizeof(struct svm_value_t);
}

#endif /* ifndef SVM_VALUE_H */
