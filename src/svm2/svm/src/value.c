
#include <svm/config.h>
#include <svm/string.h>
#include <svm/value.h>

const char *svm_value_tag_name(enum svm_value_tag_t tag) {
    static const char *mapping[] = {
        [SVM_VALUE_TAG_VOID]         = "void",
        [SVM_VALUE_TAG_BOOLEAN]      = "boolean",
        [SVM_VALUE_TAG_CHAR]         = "char",
        [SVM_VALUE_TAG_NUMBER]       = "number",
        [SVM_VALUE_TAG_STRING]       = "string",
        [SVM_VALUE_TAG_SYMBOL]       = "symbol",
        [SVM_VALUE_TAG_EMPTYLIST]    = "emptylist",
        [SVM_VALUE_TAG_CONS]         = "cons",
        [SVM_VALUE_TAG_BOX]          = "box",
        [SVM_VALUE_TAG_VECTOR]       = "vector",
        [SVM_VALUE_TAG_FUNCTION]     = "function",
        [SVM_VALUE_TAG_CLOSURE]      = "closure",
        [SVM_VALUE_TAG_CONTINUATION] = "continuation"
    };
    if (!SVM_BETWEEN(tag, 0, SVM_ARRAY_SIZE(mapping))) {
        return "unknown svm tag";
    } else {
        return mapping[tag];
    }
}

void svm_value_print(struct svm_value_t *val, FILE *outfile)
{
    switch (val->tag)
    {
        case SVM_VALUE_TAG_VOID:
            fprintf(outfile, "#<void>");
            return;
        case SVM_VALUE_TAG_BOOLEAN:
            if (val->rep.as_boolean)
                fprintf(outfile, "#t");
            else
                fprintf(outfile, "#f");
            return;
        case SVM_VALUE_TAG_CHAR: {
            char c = val->rep.as_char;
            if (isprint(c)) {
                fprintf(outfile, "#\\%c", c);
            } else if (c == ' ') {
                fprintf(outfile, "#\\space");
            } else if (c == '\t') {
                fprintf(outfile, "#\\tab");
            } else if (c == '\n') {
                fprintf(outfile, "#\\newline");
            } else {
                fprintf(outfile, "#\\x%x", c);
            }
            return;
        }
        case SVM_VALUE_TAG_NUMBER: {
            double n = val->rep.as_number;
            if (isnan(n))
                fprintf(outfile, "+nan.0");
            else if (isinf(n))
                fprintf(outfile, "%cinf.0", n > 0 ? '+' : '-');
            else
                fprintf(outfile, "%lf", val->rep.as_number);
            return;
        }
        case SVM_VALUE_TAG_STRING:
            fprintf(outfile, "\"%s\"", val->rep.as_string->bytes);
            return;
        case SVM_VALUE_TAG_SYMBOL:
            fprintf(outfile, "%s", val->rep.as_symbol->bytes);
            return;
        case SVM_VALUE_TAG_EMPTYLIST:
            fprintf(outfile, "()");
            return;
        case SVM_VALUE_TAG_CONS:
            fprintf(outfile, "(");
            svm_value_print(&val->rep.as_cons->car, outfile);
            fprintf(outfile, ".");
            svm_value_print(&val->rep.as_cons->cdr, outfile);
            fprintf(outfile, ")");
            return;
        case SVM_VALUE_TAG_BOX:
            fprintf(outfile, "(make-box ");
            svm_value_print(&val->rep.as_box->val, outfile);
            fprintf(outfile, ")");
            return;
        case SVM_VALUE_TAG_VECTOR:
            fprintf(outfile, "#(");
            struct svm_value_t *it;
            svm_vector_foreach(it, &val->rep.as_vector->vec)
                svm_value_print(it, outfile);
            fprintf(outfile, ")");
            return;
        case SVM_VALUE_TAG_FUNCTION:
            fprintf(outfile, "#<procedure %p>", (void *)&val->rep.as_function);
            return;
        case SVM_VALUE_TAG_CLOSURE:
            fprintf(outfile, "#<closure %p>", (void *)&val->rep.as_closure);
            return;
        /* case SVM_VALUE_TAG_BLOCK: */
            /* fprintf(outfile, "#<block %p>", (void *)&val->rep.as_block); */
            /* return; */
        /* case SVM_VALUE_TAG_TABLE: */
            /* fprintf(outfile, "#<table %p>", (void *)&val->rep.as_table); */
            /* return; */
        case SVM_VALUE_TAG_CONTINUATION:
            fprintf(outfile, "#<continuation %p>", (void *)&val->rep.as_continuation);
            return;
        default:
            fprintf(outfile, "#<unknown (tag = %d)>\n", val->tag);
            return;
    }
}

/* From libstdc++ */
static uint32_t hash_bytes(const void *bytes, size_t len, size_t seed)
{
    const uint32_t m = UINT32_C(0x5bd1e995);
    uint32_t hash = seed ^ len;
    const uint8_t *buf = bytes;

    while (len >= 4)
    {
        uint32_t k;
        memcpy(&k, buf, sizeof k);
        k *= m;
        k ^= k >> 24;
        k *= m;
        hash *= m;
        hash ^= k;
        buf += 4;
        len -= 4;
    }

    switch (len)
    {
        case 3:
            hash ^= buf[2] << 16;
            /* fallthrough */
        case 2:
            hash ^= buf[1] << 8;
            /* fallthrough */
        case 1:
            hash ^= buf[0];
            hash *= m;
    }

    hash ^= hash >> 13;
    hash *= m;
    hash ^= hash >> 15;
    return hash;
}

static uint32_t hash_pointer(const void *p)
{
    return hash_bytes(&p, sizeof p, UINT32_C(0xc70f6907));
}

uint32_t svm_value_hash(struct svm_value_t *val)
{
    switch (val->tag)
    {
        case SVM_VALUE_TAG_VOID:
            return 0x6f909293;
        case SVM_VALUE_TAG_EMPTYLIST:
            return 0x2a15a97c;
        case SVM_VALUE_TAG_BOOLEAN:
            return val->rep.as_boolean ? 0x14db0c2b : 0x3f6d7a1c;
        case SVM_VALUE_TAG_NUMBER:
            return val->rep.as_number == 0 ? 0 : hash_bytes(&val->rep.as_number, sizeof val->rep.as_number, UINT32_C(0xc70f6907));
        case SVM_VALUE_TAG_CHAR:
            return (uint32_t)val->rep.as_char;
        case SVM_VALUE_TAG_STRING:
            return svm_string_hash(val->rep.as_string);
        case SVM_VALUE_TAG_SYMBOL:
            return svm_string_hash(val->rep.as_symbol);
        case SVM_VALUE_TAG_CONS:
            return hash_pointer(val->rep.as_cons);
        case SVM_VALUE_TAG_BOX:
            return hash_pointer(val->rep.as_cons);
        case SVM_VALUE_TAG_VECTOR:
            return hash_pointer(val->rep.as_vector);
        case SVM_VALUE_TAG_FUNCTION:
            return hash_pointer(val->rep.as_function);
        case SVM_VALUE_TAG_CLOSURE:
            return hash_pointer(val->rep.as_closure);
        case SVM_VALUE_TAG_CONTINUATION:
            return hash_pointer(val->rep.as_continuation);
    }
}
bool svm_value_eq(const struct svm_value_t *a, const struct svm_value_t *b)
{
    if (a->tag != b->tag) {
        return false;
    }
    switch (a->tag)
    {
        case SVM_VALUE_TAG_VOID:
        case SVM_VALUE_TAG_EMPTYLIST:
            return true;
        case SVM_VALUE_TAG_BOOLEAN:
            return a->rep.as_boolean == b->rep.as_boolean;
        case SVM_VALUE_TAG_CHAR:
            return a->rep.as_char == b->rep.as_char;
        case SVM_VALUE_TAG_NUMBER:
            return a->rep.as_number == b->rep.as_number;
        case SVM_VALUE_TAG_STRING:
            return svm_string_equal(a->rep.as_string, b->rep.as_string);
        case SVM_VALUE_TAG_SYMBOL:
            return svm_string_equal(a->rep.as_symbol, b->rep.as_symbol);
        case SVM_VALUE_TAG_CONS:
            return a->rep.as_cons == b->rep.as_cons;
        case SVM_VALUE_TAG_BOX:
            return a->rep.as_box == b->rep.as_box;
        case SVM_VALUE_TAG_VECTOR:
            return a->rep.as_vector == b->rep.as_vector;
        case SVM_VALUE_TAG_FUNCTION:
            return a->rep.as_function == b->rep.as_function;
        case SVM_VALUE_TAG_CLOSURE:
            return a->rep.as_closure == b->rep.as_closure;
        case SVM_VALUE_TAG_CONTINUATION:
            return a->rep.as_continuation == b->rep.as_continuation;
    }
}
bool svm_value_equal(const struct svm_value_t *a, const struct svm_value_t *b)
{
    if (a->tag != b->tag) {
        return false;
    }
    switch (a->tag)
    {
        case SVM_VALUE_TAG_VOID:
        case SVM_VALUE_TAG_EMPTYLIST:
            return true;
        case SVM_VALUE_TAG_BOOLEAN:
            return a->rep.as_boolean == b->rep.as_boolean;
        case SVM_VALUE_TAG_CHAR:
            return a->rep.as_char == b->rep.as_char;
        case SVM_VALUE_TAG_NUMBER:
            return a->rep.as_number == b->rep.as_number;
        case SVM_VALUE_TAG_STRING:
            return svm_string_equal(a->rep.as_string, b->rep.as_string);
        case SVM_VALUE_TAG_SYMBOL:
            return svm_string_equal(a->rep.as_symbol, b->rep.as_symbol);
        case SVM_VALUE_TAG_CONS: {
            return svm_value_equal(&a->rep.as_cons->car, &b->rep.as_cons->car)
                && svm_value_equal(&a->rep.as_cons->cdr, &b->rep.as_cons->cdr);
        }
        case SVM_VALUE_TAG_BOX:
            return svm_value_equal(&a->rep.as_box->val, &b->rep.as_box->val);
        case SVM_VALUE_TAG_VECTOR: {
            svm_vector_t(struct svm_value_t) *veca = (void *)&a->rep.as_vector->vec;
            svm_vector_t(struct svm_value_t) *vecb = (void *)&b->rep.as_vector->vec;
            if (svm_vector_size(veca) != svm_vector_size(vecb))
                return false;
            for (size_t i = 0; i < svm_vector_size(veca); i++)
                if (!svm_value_equal(&svm_vector_at(veca, i), &svm_vector_at(vecb, i)))
                    return false;
            return true;
        }
        case SVM_VALUE_TAG_FUNCTION:
            return a->rep.as_function == b->rep.as_function;
        case SVM_VALUE_TAG_CLOSURE:
            return a->rep.as_closure == b->rep.as_closure;
        case SVM_VALUE_TAG_CONTINUATION:
            return a->rep.as_continuation == b->rep.as_continuation;
    }
}
