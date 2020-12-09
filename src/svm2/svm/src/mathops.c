
#include <svm/config.h>
#include <svm/value.h>
#include <math.h>

static inline bool svm_number(double n)
{
    SVM_UNUSED(n);
    return true;
}

static inline bool svm_complex(double n)
{
    SVM_UNUSED(n);
    return true;
}

static inline bool svm_real(double n)
{
    SVM_UNUSED(n);
    return true;
}

static inline bool svm_rational(double n)
{
    return finite(n);
}

static inline bool svm_integer(double n)
{
    return fmod(n, 1.0) == 0.0;
}

static inline bool svm_exact(double n)
{
    return (int32_t)n == n;
}

static inline bool svm_inexact(double n)
{
    return !svm_exact(n);
}

static inline bool svm_exact_integer(double n)
{
    return svm_exact(n);
}

static inline bool svm_finite(double n)
{
    return finite(n);
}

static inline bool svm_infinite(double n)
{
    return isinf(n);
}

static inline bool svm_nan(double n)
{
    return isnan(n);
}

#define b(x) ((x) ? "#t" : "#f")

int main(int argc, char *argv[])
{
    if (argc < 2)
        return 1;
    double n = strtod(argv[1], NULL);
    printf("(number? %f)        == %s\n", n, b(svm_number(n)));
    printf("(complex? %f)       == %s\n", n, b(svm_complex(n)));
    printf("(real? %f)          == %s\n", n, b(svm_real(n)));
    printf("(rational? %f)      == %s\n", n, b(svm_rational(n)));
    printf("(integer? %f)       == %s\n", n, b(svm_integer(n)));
    printf("(exact? %f)         == %s\n", n, b(svm_exact(n)));
    printf("(inexact? %f)       == %s\n", n, b(svm_inexact(n)));
    printf("(exact-integer? %f) == %s\n", n, b(svm_exact_integer(n)));
    printf("(finite? %f)        == %s\n", n, b(svm_finite(n)));
    printf("(infinite? %f)      == %s\n", n, b(svm_infinite(n)));
    printf("(nan? %f)           == %s\n", n, b(svm_nan(n)));
    return 0;
}


