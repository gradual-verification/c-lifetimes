#include <stdlib.h>

// this function should have a free effect which "kills" p,
// for example something like: (`a => dead) -> ()
static void free_ptr(int* p) {
    free(p);
}