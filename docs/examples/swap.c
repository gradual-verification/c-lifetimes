#include <stdlib.h>

// we want qa "lifetime effect," for example an annotation like the following:
// ('a => 'b) -> ('b => 'a) -> ()
static unsigned int swap(int** a, int** b) {
    int * temp = *a;
    *a = *b;
    *b = temp;
}