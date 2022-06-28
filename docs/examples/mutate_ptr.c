#include <stdlib.h>

static void mutate_ptr(int** p) {
    *p = (int*) malloc(sizeof(int));
}