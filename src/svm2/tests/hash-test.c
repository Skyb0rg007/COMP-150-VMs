
#include <svm/config.h>
#include <svm/hash.h>
#include <cmocka.h>

static struct svm_allocator_t alloc = {
    .fun = svm_default_allocator,
    .ud = NULL
};

static void test1(void **state)
{
    SVM_UNUSED(state);
    svm_hash_t h[1];
    svm_hash_init(h);

    for (int i = 0; i < 250000; i++) {
        int key = i;
        int val = i;
        int ret;
        uint32_t pos = svm_hash_put(h, key, &ret, &alloc);
        assert_true(pos != svm_hash_end(h));
        assert_int_equal(ret, 1);
        svm_hash_val(h, pos) = val;

        if ((rand() % 10) == 0) {
            uint32_t pos = svm_hash_get(h, key);
            assert_true(pos != svm_hash_end(h));
            svm_hash_del(h, pos);
        }
    }

    uint32_t bucket;
    int key, val;
    svm_hash_foreach(bucket, key, val, h) {
        /* printf("h[%d] = %d\n", key, val); */
        assert_int_equal(key, val);
    }

    svm_hash_free(h, &alloc);
}

int main(void)
{
    srand(time(NULL));
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test1)
    };
    return cmocka_run_group_tests_name("hash tests", tests, NULL, NULL);
}

