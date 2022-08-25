typedef struct {
    char * bottom;
    char * top;
    int size;
    int capacity;
} stack_t;

void stack_init(stack_t *s, int len) 
// s.bottom => `a, s.top => 'a where 'a = alloc()
{
    s->bottom = malloc(len);
    s->top = s->bottom;
}

void stack_free(stack_t *s) 
// s.bottom => dead, s.top => dead
{
    free(s->bottom);
}

void use_stack(){

    stack_t s;
    stack_init(&s, 16);
    stack_free(&s);
}
