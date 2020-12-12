
#include <svm/config.h>
#include <svm/vector.h>
#include <cmocka.h>

static struct svm_allocator_t a = {
    .fun = svm_default_allocator,
    .ud = NULL
};

static void test1(void **state)
{
    int n = 200;
    SVM_UNUSED(state);
    svm_vector_t(int) vec;
    svm_vector_init(&vec);

    for (int i = 0; i < n; i++) {
        assert_int_equal(svm_vector_size(&vec), i);
        svm_vector_push_back(&vec, i, &a);
        assert_int_equal(svm_vector_size(&vec), i + 1);
    }

    assert_int_equal(svm_vector_size(&vec), n);

    for (int i = 0; i < n; i++)
        svm_vector_pop_back(&vec);

    assert_true(svm_vector_empty(&vec));
    svm_vector_free(&vec, &a);
}

int main(void)
{
    struct CMUnitTest tests[] = {
        cmocka_unit_test(test1)
    };
    return cmocka_run_group_tests_name("vector tests", tests, NULL, NULL);
}
