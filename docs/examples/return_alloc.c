#include <stdlib.h>

static int* return_alloc() {
    int* p = (int*) malloc(sizeof(int));
    return p;
}