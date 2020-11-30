
#include <svm/config.h>
#include <svm/assert.h>
#include <svm/alloc.h>

void *svm_default_allocator(void *p, size_t oldsz, size_t newsz, void *ud)
{
    (void)oldsz;
    (void)ud;
    if (newsz == 0) {
        free(p);
        return NULL;
    } else {
        void *new = realloc(p, newsz);
        svm_assert(new);
        return new;
    }
}

