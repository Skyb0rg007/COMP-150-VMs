
#include <svm/vm.h>
#include <svm/heap.h>
#include <cmocka.h>

static int setup_vm(void **state)
{
    static struct svm_vm_t vm;
    svm_vm_init(&vm);
    *state = &vm;
    return 0;
}

static int teardown_vm(void **state)
{
    svm_vm_free(*state);
    *state = NULL;
    return 0;
}

static void test1(void **state)
{
    struct svm_vm_t *vm = *state;
    for (int i = 0; i < 100; i++) {
        void *p = svm_heap_alloc(vm, 128);
        assert_ptr_not_equal(p, NULL);
    }
}

int main(void)
{
    const struct CMUnitTest tests[] = {
        cmocka_unit_test_setup_teardown(test1, setup_vm, teardown_vm)
    };
    return cmocka_run_group_tests_name("heap tests", tests, NULL, NULL);
}

