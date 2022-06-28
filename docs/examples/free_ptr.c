#include <stdlib.h>

static void free_ptr(int* p) {
    free(p);
}