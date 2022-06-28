#include <stdlib.h>


// this function should be total (no memory effects)
// since it frees all the memory it allocates
static void loop_alloc(int n) {
    int** ptrs = (int**) malloc(n * sizeof(int*));
    for (int i = 0; i < n; i++) {
        ptrs[i] = (int*) malloc(sizeof(int));
    }

    for (int i = 0; i < n; i++) {
        free(ptrs[i]);
    }

    free(ptrs);
} 