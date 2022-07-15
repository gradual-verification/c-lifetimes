
// ERROR: we created a dangling pointer by 20 <- 14
int main() {
    int * * x = alloc(sizeof(int*));
    int * y = alloc(sizeof(int));
    equalize(x, y);
    free(y);
    printf("%d", **x);
    return 0;
}

// & 'a & 'b i32, & 'c i32, where 'c:'b
void equalize(int * * x, int * y) {    
    *x = y;   
}



//Stage 1:

//  1: intraprocedural analysis of equalize()


//  2: intraprocedural analysis of main()


//Stage 2

// Using constraints to detect that we have a use after free