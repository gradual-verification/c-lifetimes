#include <stdlib.h>

// should have an alloc effect which returns something of variable lifetime
static int* return_alloc() {
    int* p = (int*) malloc(sizeof(int));
    return p;
}