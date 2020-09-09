// The virtual-machine heap, eventually to be garbage-collected.

// Module 1 onward: Use for any data structure that is allocated by
// a VM instruction: all those structures are eventually supposed to be 
// garbage collected.  

// This interface is to be used directly to allocate blocks,
// functions, and closures.  It is also used indirectly by functions
// the create VMString, VTable_T, and VSeq_T.

#ifndef VMHEAP_INCLUDED
#define VMHEAP_INCLUDED

#include <stddef.h>
#include <stdbool.h>


//// initialization and finalization

extern void heap_init(void);
extern void heap_shutdown(void);
  // returns all memory to the OS (to make valgrind happy)


//// allocators, which never return NULL

extern void *vmalloc(size_t);
extern void *vmcalloc(size_t, size_t);
   // modules 11 and 12 will add ability to recover this memory

extern bool vmalloc_islarge(size_t);
  // tells string client when not to intern

#endif /* VMHEAP_INCLUDED */
