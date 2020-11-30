
/* TODO: use something other than sscanf
 * Ex. use a non-destructive tokenizer function
 */
#include <svm/config.h>
#include <svm/vm.h>
#include <svm/string.h>
#include <svm/parse.h>
#include <svm/heap.h>

/* A non-destructive strtok */
static const char *tok(const char **rest, size_t *len);
/* Like getline, but specialized for the svm */
static ssize_t read_line(struct svm_vm_t *vm, char **lineptr, size_t *n, FILE *input);
/* Parse a literal, returning its lit index. Or panic. */
static uint32_t scan_literal(struct svm_vm_t *vm, char *line);
/* Parse an instruction, or panic. */
static svm_instruction_t scan_instruction(struct svm_vm_t *vm, int *nregs, char *line);
/* Parse a function body, or panic. */
static struct svm_function_t *parse_function(struct svm_vm_t *vm, int arity, int count, FILE *input);

/* Parses the given file as a module.
 * Returns a literal-pool-allocated vm function of arity 0
 */
struct svm_function_t *svm_parse_module(struct svm_vm_t *vm, FILE *input)
{
    char *line = NULL;
    ssize_t len;
    size_t allocated = 0;

    len = read_line(vm, &line, &allocated, input);
    if (len < 0) {
        svm_panic("Expected module, got EOF");
    }

    int n;
    unsigned count;
    if (sscanf(line, ".load module %u %n", &count, &n) == 1 && line[n] == '\0') {
        svm_free(&vm->allocator, line, allocated);
        return parse_function(vm, 0, count, input);
    } else {
        svm_panic("Expected module, got \"%s\"", line);
    }
}

/* Convert a token to an integer, or panic */
static long tolong(const char *str, size_t len)
{
    char *end;
    long n = strtol(str, &end, 10);
    if (n == LONG_MIN)
        svm_panic("Integer underflow in constant %.*s", (int)len, str);
    if (n == LONG_MAX)
        svm_panic("Integer overflow in constant %.*s", (int)len, str);
    if (end != str + len) 
        svm_panic("Unable to convert \"%.*s\" to an integer", (int)len, str);
    return n;
}

/* Compare a token to a null-terminated string */
static inline bool tokeq(const char *tok, size_t len, const char *str)
{
    return strlen(str) == len && memcmp(tok, str, len) == 0;
}

static svm_instruction_t scan_instruction(struct svm_vm_t *vm, int *nregs, char *line)
{
    const char *rest = line;
    size_t len;
    #define error()           svm_panic("Invalid line: \"%s\".", line)
    #define error_usage(desc) svm_panic("Invalid line: \"%s\". Usage: \"%s\".", line, desc)

    const char *command = tok(&rest, &len);
    if (command == NULL)
        error();

    #define handle_R0(upper, desc)                                \
        if (tok(&rest, NULL))                                     \
            error();                                              \
        return svm_instruction_r0(SVM_OPCODE_##upper);

    #define handle_R1(upper, desc)                                \
        size_t xlen;                                              \
        const char *xtok = tok(&rest, &xlen);                     \
        if (tok(&rest, NULL))                                     \
            error_usage(desc);                                    \
        int8_t x = tolong(xtok, xlen);                            \
        *nregs = SVM_MAX(*nregs, x);                              \
        return svm_instruction_r1(SVM_OPCODE_##upper, x);

    #define handle_R2(upper, desc)                                \
        size_t xlen, ylen;                                        \
        const char *xtok = tok(&rest, &xlen);                     \
        const char *ytok = tok(&rest, &ylen);                     \
        if (tok(&rest, NULL))                                     \
            error_usage(desc);                                    \
        int8_t x = tolong(xtok, xlen);                            \
        int8_t y = tolong(ytok, ylen);                            \
        *nregs = SVM_MAX(*nregs, x);                              \
        *nregs = SVM_MAX(*nregs, y);                              \
        return svm_instruction_r2(SVM_OPCODE_##upper, x, y);

    #define handle_R3(upper, desc)                                \
        size_t xlen, ylen, zlen;                                  \
        const char *xtok = tok(&rest, &xlen);                     \
        const char *ytok = tok(&rest, &ylen);                     \
        const char *ztok = tok(&rest, &zlen);                     \
        if (tok(&rest, NULL))                                     \
            error_usage(desc);                                    \
        int8_t x = tolong(xtok, xlen);                            \
        int8_t y = tolong(ytok, ylen);                            \
        int8_t z = tolong(ztok, zlen);                            \
        *nregs = SVM_MAX(*nregs, x);                              \
        *nregs = SVM_MAX(*nregs, y);                              \
        *nregs = SVM_MAX(*nregs, z);                              \
        return svm_instruction_r3(SVM_OPCODE_##upper, x, y, z);

    #define handle_R1LIT(upper, desc)                             \
        size_t xlen;                                              \
        const char *xtok = tok(&rest, &xlen);                     \
        int8_t x = tolong(xtok, xlen);                             \
        int32_t lit = scan_literal(vm, (char *)rest);             \
        *nregs = SVM_MAX(*nregs, x);                              \
        return svm_instruction_r1lit(SVM_OPCODE_##upper, x, lit);

    #define handle_R0I24(upper, desc)                             \
        size_t xyzlen;                                            \
        const char *xyztok = tok(&rest, &xyzlen);                 \
        if (tok(&rest, NULL))                                     \
            error_usage(desc);                                    \
        int32_t xyz = tolong(xyztok, xyzlen);                     \
        return svm_instruction_r0i24(SVM_OPCODE_##upper, xyz);

    #define X(lower, _title, upper, format, desc) \
        if (tokeq(command, len, #lower)) {        \
            handle_##format(upper, desc)          \
        }
    SVM_FOREACH_OPCODE(X)
    #undef X

    error();

    #undef error
    #undef error_usage
    #undef handle_R0
    #undef handle_R1
    #undef handle_R2
    #undef handle_R3
    #undef handle_R1LIT
    #undef handle_R0I24
}

/* 
static svm_instruction_t scan_instruction(struct svm_vm_t *vm, int *nregs, char *line)
{
    [>Number of chars left in the line - written by sscanf<]
    int n = 0;
    [>Instruction parts<]
    int8_t x, y, z;
    int32_t xyz;
    [>Testers for each type of opcode<]
    #define test_R0(lower, upper)                                                                           \
        if (sscanf(line, #lower " %n", &n) == 0 && !line[n]) {                                              \
            return svm_instruction_r0(SVM_OPCODE_##upper);                                                  \
        }
    #define test_R1(lower, upper)                                                                           \
        if (sscanf(line, #lower " %" SCNd8 " %n", &x, &n) == 1 && !line[n]) {                               \
            *nregs = SVM_MAX(*nregs, x+1);                                                                      \
            return svm_instruction_r1(SVM_OPCODE_##upper, x);                                               \
        }
    #define test_R2(lower, upper)                                                                           \
        if (sscanf(line, #lower " %" SCNd8 " %" SCNd8 " %n", &x, &y, &n) == 2 && !line[n]) {                \
            *nregs = SVM_MAX(*nregs, SVM_MAX(x, y)+1);                                                              \
            return svm_instruction_r2(SVM_OPCODE_##upper, x, y);                                            \
        }
    #define test_R3(lower, upper)                                                                           \
        if (sscanf(line, #lower " %" SCNd8 " %" SCNd8 " %" SCNd8 " %n", &x, &y, &z, &n) == 3 && !line[n]) { \
            *nregs = SVM_MAX(*nregs, SVM_MAX(x, SVM_MAX(y, z))+1);                                                      \
            return svm_instruction_r3(SVM_OPCODE_##upper, x, y, z);                                         \
        }
    #define test_R1LIT(lower, upper)                                                                        \
        if (sscanf(line, #lower " %" SCNd8 " %n", &x, &n) == 1) {                                           \
            *nregs = SVM_MAX(*nregs, x+1);                                                                      \
            line += n;                                                                                      \
            int16_t yz = scan_literal(vm, line);                                                            \
            return svm_instruction_r1lit(SVM_OPCODE_##upper, x, yz);                                        \
        }
    #define test_R0I24(lower, upper)                                                                        \
        if (sscanf(line, #lower " %" SCNd32 " %n", &xyz, &n) == 1 && !line[n]) {                            \
            return svm_instruction_r2(SVM_OPCODE_##upper, x, y);                                            \
        }

    [>Test each opcode in order<]
    #define X(lower, _title, upper, format, _desc) \
        test_##format(lower, upper)
    SVM_FOREACH_OPCODE(X)
    #undef X

    [>If this line is reached, the input is invalid and we *will* abort<]

    [>Test each opcode in order for error message<]
    #define X(lower, _title, upper, format, desc)                     \
        if (strncmp(#lower, line, strlen(#lower)) == 0) {             \
            line[strlen(line) - 1] = '\0';                            \
            svm_panic("Invalid line: \"%s\". Usage: %s", line, desc); \
        }
    SVM_FOREACH_OPCODE(X)
    #undef X

    svm_panic("Invalid line: \"%s\".", line);
}
 */
static uint32_t scan_literal(struct svm_vm_t *vm, char *literal)
{
    char *line = literal;
    struct svm_value_t val;
    int n = 0;
    unsigned len;
    double f;
    if (sscanf(line, " emptylist %n", &n) == 0 && !line[n]) {
        svm_value_set_emptylist(&val);
    } else if (sscanf(line, " true %n", &n) == 0 && !line[n]) {
        svm_value_set_boolean(&val, true);
    } else if (sscanf(line, " false %n", &n) == 0 && !line[n]) {
        svm_value_set_boolean(&val, false);
    } else if (sscanf(line, " void %n", &n) == 0 && !line[n]) {
        svm_value_set_void(&val);
    } else if (sscanf(line, " string %u %n", &len, &n) == 1) {
        line += n;
        char *tmp = svm_alloc(&vm->allocator, len + 1);
        for (unsigned i = 0; i < len; i++) {
            int c = 0;
            sscanf(line, "%d %n", &c, &n);
            tmp[i] = c;
            line += n;
        }
        tmp[len] = '\0';
        if (line[0] != '\0')
            svm_panic("Invalid string literal: \"%s\"", literal);
        svm_value_set_string(&val, svm_string_new(vm, tmp, len));
        svm_free(&vm->allocator, tmp, len + 1);
    } else if (sscanf(line, " %lf %n", &f, &n) && !line[n]) {
        svm_value_set_number(&val, f);
    } else {
        svm_panic("Invalid literal: \"%s\"", literal);
    }

    return svm_vm_pushliteral(vm, val);
}

/* Read a line from the file */
static ssize_t read_line(struct svm_vm_t *vm, char **lineptr, size_t *n, FILE *input)
{
    ssize_t result;
    size_t cur_len = 0;

    svm_assert(lineptr);
    svm_assert(n);
    svm_assert(input);

    flockfile(input);

    if (*lineptr == NULL) {
        *n = 120;
        *lineptr = svm_alloc(&vm->allocator, *n);
        svm_assert_release(*lineptr);
    }

    for (;;)
    {
        int i = getc_unlocked(input);
        /* Skip comments */
        if (cur_len == 0 && i == '#') {
            do {
                i = getc_unlocked(input);
            } while (i != '\n' && i != EOF);
            continue;
        }
        if (i == EOF) {
            /* This is overridden if cur_len is nonzero */
            result = -1;
            break;
        }
        /* Don't include newline in return value
         * \n, \r, and \0 are all considered newlines
         */
        if (i == '\n' || i == '\r' || i == '\0') {
            /* Skip empty lines */
            if (cur_len == 0) {
                continue;
            } else {
                break;
            }
        }
        /* Skip leading whitespace */
        if (cur_len == 0 && isspace(i)) {
            continue;
        }

        /* Do we need to expand? */
        if (cur_len + 1 >= *n) {
            size_t needed_max =
                SSIZE_MAX < SIZE_MAX ? (size_t)SSIZE_MAX + 1 : SIZE_MAX;
            size_t needed = 2 * *n + 1;
            
            if (needed > needed_max)
                needed = needed_max;
            if (cur_len + 1 >= needed) {
                result = -1;
                goto unlock_return;
            }

            *lineptr = svm_realloc(&vm->allocator, *lineptr, *n, needed);
            svm_assert_release(*lineptr);
            *n = needed;
        }

        (*lineptr)[cur_len++] = i;
    }
    (*lineptr)[cur_len] = '\0';
    result = cur_len ? (ssize_t)cur_len : result;

unlock_return:
    funlockfile(input);
    return result;
}

static struct svm_function_t *parse_function(struct svm_vm_t *vm, int arity, int count, FILE *input)
{
    char *line = NULL;
    ssize_t len;
    size_t allocated = 0;

    struct svm_function_t *fun = svm_heap_alloc(vm, svm_function_allocsize(count + 1));
    fun->forwarded = NULL;
    fun->arity = arity;
    fun->size = count + 1;
    fun->nregs = 0;

    for (int i = 0; i < count; i++) {
        len = read_line(vm, &line, &allocated, input);
        if (len < 0) {
            svm_panic("Expected instruction, got EOF");
        }
        int reg, n, count;
        if (sscanf(line, ".load function %d %d %d %n", &reg, &arity, &count, &n) && !line[n]) {
            struct svm_value_t val;
            svm_value_set_function(&val, parse_function(vm, arity, count, input));
            fun->instructions[i] =
                svm_instruction_r1lit(
                        SVM_OPCODE_LOADLITERAL,
                        reg,
                        svm_vm_pushliteral(vm, val));
        } else {
            fun->instructions[i] = scan_instruction(vm, &fun->nregs, line);
        }
    }
    if (line)
        svm_free(&vm->allocator, line, allocated);
    fun->instructions[count] = svm_instruction_r0(SVM_OPCODE_HALT);
    return fun;
}

static const char *tok(const char **rest, size_t *len)
{
    const char *t = NULL;
    size_t l = 0;

    while (**rest && isspace(**rest))
        (*rest)++;

    if (**rest) {
        t = *rest;
        while (**rest && !isspace(**rest))
            (*rest)++, l++;
    }
    if (len)
        *len = l;
    return t;
}

