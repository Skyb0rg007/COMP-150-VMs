
/* TODO */
#include <svm/config.h>
#include <svm/string.h>
#include <svm/value.h>

struct svm_symbol_t {
    struct svm_symbol_t *forwarded;
    size_t length, hash;
    char bytes[];
};

struct svm_symbolcache_t {
    size_t population, nbuckets;
    struct svm_symbol_t **buckets;
};

void svm_symbolcache_init(struct svm_symbolcache_t *cache)
{
}


