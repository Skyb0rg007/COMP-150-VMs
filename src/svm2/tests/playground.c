
#include <svm/config.h>
#include <svm/vm.h>
#include <svm/heap.h>
#include <svm/instruction.h>
#include <svm/parse.h>

extern svm_instruction_t svm_parse_instruction(struct svm_vm_t *vm, FILE *input);

static const char *tok(const char **str, int *sz)
{
    *sz = 0;
    while (isspace(**str))
        (*str)++;
    if (**str) {
        const char *t = *str;
        while (!isspace(**str))
            (*str)++, (*sz)++;
        return t;
    }
    return NULL;
}

int main(int argc, char *argv[])
{
    SVM_UNUSED(argc);
    SVM_UNUSED(argv);

    printf("argv[1] = %s\n", argv[1]);

    const char *line = "  add 1   2   ";

    int size;
    const char *t = tok(&line, &size);
    printf("t = \"%.*s\", size = %d, rest = \"%s\"\n", size, t, size, line);

    t = tok(&line, &size);
    printf("t = \"%.*s\", size = %d, rest = \"%s\"\n", size, t, size, line);

    t = tok(&line, &size);
    printf("t = \"%.*s\", size = %d, rest = \"%s\"\n", size, t, size, line);

    t = tok(&line, &size);
    printf("t = \"%.*s\", size = %d, rest = \"%s\"\n", size, t, size, line);

    t = tok(&line, &size);
    printf("t = \"%.*s\", size = %d, rest = \"%s\"\n", size, t, size, line);

    /* char line[] = " add 1   2  "; */

    /* char *save; */
    /* char *tok1 = strtok_r(line, " ", &save); */
    /* printf("tok1 = '%s'\n", tok1); */
    /* char *tok2 = strtok_r(NULL, " ", &save); */
    /* printf("tok2 = '%s', save = '%s'\n", tok2, save); */
    /* char *tok3 = strtok_r(NULL, " ", &save); */
    /* printf("tok3 = '%s'\n", tok3); */
    /* char *tok4 = strtok_r(NULL, " ", &save); */
    /* printf("tok4 = '%s'\n", tok4); */


    /* static struct svm_vm_t vm; */
    /* svm_vm_init(&vm); */

    /* struct svm_function_t *f = svm_parse_module(&vm, stdin); */
    /* SVM_UNUSED(f); */

    /* svm_vm_free(&vm); */

    /* ssize_t read_line(struct svm_vm_t *vm, char **lineptr, size_t *n, FILE *input); */

    /* ssize_t len; */
    /* char *lineptr = NULL; */
    /* size_t n = 0; */


    /* while ((len = read_line(&vm, &lineptr, &n, stdin)) > 0) */
    /* { */
        /* printf("len = %zu, line = \"%s\"\n", len, lineptr); */
    /* } */
    /* svm_free(&vm, lineptr, len); */
    /* svm_vm_free(&vm); */

    /* svm_log("vm initialized"); */
    /* svm_instruction_t ret = svm_parse_instruction(&vm, stdin); */
    /* printf("ret = %#x\n", ret); */

    /* int n; */
    /* int ret = scanf("foo %n", &n); */
    /* printf("ret = %d, n = %d\n", ret, n); */

/* 
    if (!argv[1])
        return 1;

    for (int i = 0; i < (int)strlen(argv[1]); i++)
        if (isprint(argv[1][i])) {
            fputc(argv[1][i], stdout);
        } else {
            printf("\\x%X;", argv[1][i]);
        }
    puts("");

    int n = 0;
    char buf[120] = {0};
    int ret = sscanf(argv[1], " command %d %[^\n%]", &n, buf);
    printf("ret = %d, n = %d, buf = \"%s\"\n", ret, n, buf);
 */
    return 0;
}

