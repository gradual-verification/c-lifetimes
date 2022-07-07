#include <stdlib.h>

/*
predicate alloc_list(int[] elements, int index){
    if(index == 0){
        true
    }else{
        alloc(elements[index-1]) and alloc_list(index - 1)
    }
}
*/

/*
predicate free_list(int[] elements, int index){
    if(index == 0){
        true
    }else{
        free(elements[index-1]) and free_list(index - 1)
    }
}
*/

static void loop_alloc(int n) 
// (a -> void) : total
{
    int** ptrs = (int**) malloc(n * sizeof(int*));

    //@fold alloc_list(ptrs, 0);
    for (int i = 0; i < n; i++) 
    //@loop_invariant alloc_list(ptrs, index);
    {
        //unfold alloc_list(ptrs, i);
        ptrs[i] = (int*) malloc(sizeof(int)); // + alloc(ptrs[i])
        //fold alloc_list(ptrs, i+1);
    }  

    //@fold free_list(ptrs, 0);
    for (int i = 0; i < n; i++) 
    //@loop_invariant free_list(ptrs, index)
    {
        //unfold free_list(ptrs, i);
        free(ptrs[i]);                      // + free(ptrs[i])
        //unfold free_list(ptrs, i+1);
    }
}