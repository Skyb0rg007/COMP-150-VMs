/* TODO: redo this once the heap is ported
 * The stringtable doesn't free the interned strings,
 * so we can't perform memory testing here.
 */

#include <svm/vm.h>
#include <svm/string.h>
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

static void small_alloc(void **state)
{
    struct svm_string_t *str, *foo;
    const char *a = "abcd";
    size_t len = strlen(a);
    foo = svm_string_new(*state, a, len);
    assert_non_null(foo);
    for (int i = 0; i < 200; i++) {
        str = svm_string_new(*state, a, len);
        assert_non_null(str);
        /* Small allocations are interned */
        assert_ptr_equal(str, foo);
    }
}

static void large_alloc(void **state)
{
    struct svm_string_t *str, *foo;
    const char *a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    size_t len = strlen(a);
    foo = svm_string_new(*state, a, len);
    assert_non_null(foo);
    for (int i = 0; i < 200; i++) {
        str = svm_string_new(*state, a, len);
        assert_non_null(str);
        /* Large allocations are not interned */
        assert_ptr_not_equal(str, foo);
        assert_int_equal(str->length, foo->length);
        assert_memory_equal(str->bytes, foo->bytes, str->length + 1);
    }
}

int main(void)
{
    const struct CMUnitTest tests[] = {
        cmocka_unit_test_setup_teardown(small_alloc, setup_vm, teardown_vm),
        cmocka_unit_test_setup_teardown(large_alloc, setup_vm, teardown_vm)
    };
    return cmocka_run_group_tests_name("string tests", tests, NULL, NULL);
}

