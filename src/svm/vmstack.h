#ifndef VMSTACK_H
#define VMSTACK_H value

#include "iformat.h"
#include "value.h"

/* XXX: comment mentioning names */
struct Activation {
    struct VMFunction *fun;
    int destreg;
    int window;
};

#endif /* ifndef VMSTACK_H */
