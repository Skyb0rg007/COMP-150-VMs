
#include <svm/config.h>
#include <svm/value.h>

const char *svm_value_tag_name(enum svm_value_tag_t tag) {
    static const char *mapping[] = {
        [SVM_VALUE_TAG_VOID]      = "void",
        [SVM_VALUE_TAG_BOOLEAN]   = "boolean",
        [SVM_VALUE_TAG_CHAR]      = "char",
        [SVM_VALUE_TAG_NUMBER]    = "number",
        [SVM_VALUE_TAG_STRING]    = "string",
        [SVM_VALUE_TAG_SYMBOL]    = "symbol",
        [SVM_VALUE_TAG_EMPTYLIST] = "emptylist",
        [SVM_VALUE_TAG_CONS]      = "cons",
        [SVM_VALUE_TAG_FUNCTION]  = "function",
        [SVM_VALUE_TAG_CLOSURE]   = "closure",
        [SVM_VALUE_TAG_BLOCK]     = "block",
        [SVM_VALUE_TAG_TABLE]     = "table",
        [SVM_VALUE_TAG_CONTINUATION] = "continuation",
    };
    if (tag < 0 || tag > (int)SVM_ARRAY_SIZE(mapping)) {
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
            svm_value_print(&val->rep.as_block->slots[0], outfile);
            fprintf(outfile, ".");
            svm_value_print(&val->rep.as_block->slots[1], outfile);
            fprintf(outfile, ")");
            return;
        case SVM_VALUE_TAG_FUNCTION:
            fprintf(outfile, "#<function %p>", (void *)&val->rep.as_function);
            return;
        case SVM_VALUE_TAG_CLOSURE:
            fprintf(outfile, "#<closure %p>", (void *)&val->rep.as_closure);
            return;
        case SVM_VALUE_TAG_BLOCK:
            fprintf(outfile, "#<block %p>", (void *)&val->rep.as_block);
            return;
        case SVM_VALUE_TAG_TABLE:
            fprintf(outfile, "#<table %p>", (void *)&val->rep.as_table);
            return;
        case SVM_VALUE_TAG_CONTINUATION:
            fprintf(outfile, "#<continuation %p>", (void *)&val->rep.as_continuation);
            return;
        default:
            fprintf(outfile, "#<unknown (tag = %d)>\n", val->tag);
            return;
    }
}

