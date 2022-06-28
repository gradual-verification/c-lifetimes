#include <stdlib.h>

static unsigned int swap(int** a, int** b) {
    int * temp = *a;
    *a = *b;
    *b = temp;
}