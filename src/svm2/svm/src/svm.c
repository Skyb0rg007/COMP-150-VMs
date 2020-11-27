
#include <svm/assert.h>
#include <svm/config.h>
#include <svm/config.h>
#include <svm/instruction.h>
#include <svm/opcodes.h>
#include <svm/parse.h>
#include <svm/string.h>
#include <svm/value.h>
#include <svm/vm.h>
#include <svm/run.h>

int main(int argc, char *argv[]) {
    printf("SVM: version %s\n", SVM_VERSION);

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return 1;
    }
    static struct svm_vm_t vm[1];
    svm_vm_init(vm);

    FILE *fp = fopen(argv[1], "r");
    if (!fp) {
        fprintf(stderr, "Error opening file \"%s\": %s\n", argv[1], strerror(errno));
        return 1;
    }
    struct svm_function_t *f = svm_parse_module(vm, fp);
    svm_run(vm, f);

    svm_vm_free(vm);
    return 0;
}
