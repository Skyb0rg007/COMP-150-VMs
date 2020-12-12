
#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdbool.h>

#define bits    16
#define top2(x) (((x) & (3L << (bits - 2))) >> (bits - 2))

struct int_sqrt {
    int32_t sqrt, frac;
};

static void isqrt(int32_t x, struct int_sqrt *q)
{
    int32_t res = trunc(sqrt((double)x));
    int32_t rem = x - res * res;
    q->sqrt = res;
    q->frac = rem;
}


int main(int argc, char *argv[])
{
    if (argc < 2)
        return 1;

    double input = strtod(argv[1], NULL);
    printf("(sqrt %f) == %f\n", input, sqrt(input));
    if (trunc(input) == input) {
        struct int_sqrt q;
        isqrt(input, &q);
        printf("(exact-integer-sqrt %f) == %d %d\n", input, q.sqrt, q.frac);
    }
    printf("fmod(%f, 1.0) == %f\n", input, fmod(input, 1.0));
    printf("((int32_t)%f == %f) == %s\n", input, input, (int32_t)input == input ? "#t" : "#f");

    printf("%d\n", DBL_MANT_DIG);
    printf("%f\n", 9007199254740993.0 + 2);
    /* for (double n = pow(2, 53); ; n++) { */
        /* bool isInt = fmod(n, 1.0) == 0.0 ; */
        /* if (!isInt) */
        /* { */
            /* printf("%f\n", n); */
            /* break; */
        /* } */
    /* } */
    return 0;
}

