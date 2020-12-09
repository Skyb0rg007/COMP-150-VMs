/* TODO: redo this once the heap is ported
 * The stringtable doesn't free the interned strings,
 * so we can't perform memory testing here.
 */

#include <svm/vm.h>
#include <svm/string.h>
#include <cmocka.h>

static int group_setup(void **state)
{
    static struct svm_vm_t vm;
    svm_vm_init(&vm);
    *state = &vm;
    return 0;
}

static int group_teardown(void **state)
{
    svm_vm_free(*state);
    *state = NULL;
    return 0;
}

static int test_setup(void **state)
{
    svm_vm_gc(*state);
    return 0;
}

static int test_teardown(void **state)
{
    svm_vm_gc(*state);
    return 0;
}

static void small_alloc(void **state)
{
    const char *a = "abcd";
    size_t len = strlen(a);
    struct svm_string_t *foo = svm_string_new(*state, a, len);
    assert_non_null(foo);
    for (int i = 0; i < 200; i++) {
        struct svm_string_t *str =
         svm_string_new(*state, a, len);
        assert_non_null(str);
        /* Small allocations are interned */
        assert_ptr_equal(str, foo);
    }
}

static void large_alloc(void **state)
{
    const char *a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
    size_t len = strlen(a);
    struct svm_string_t *foo = svm_string_new(*state, a, len);
    assert_non_null(foo);
    for (int i = 0; i < 200; i++) {
        struct svm_string_t *str = svm_string_new(*state, a, len);
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
        cmocka_unit_test_setup_teardown(small_alloc, test_setup, test_teardown),
        cmocka_unit_test_setup_teardown(large_alloc, test_setup, test_teardown)
    };
    return cmocka_run_group_tests_name("string tests", tests, group_setup, group_teardown);
}

