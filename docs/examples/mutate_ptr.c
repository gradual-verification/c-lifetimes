#include <stdlib.h>

// because we mutate *p, it is possible this causes a memory leak
// if e.g nothing else points to **p
// ideally, this mutation should be reflected as an effect
static void mutate_ptr(int** p) {
    *p = (int*) malloc(sizeof(int));
}