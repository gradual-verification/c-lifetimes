typedef struct {
    char *buf;
    int size;
    int length;
    int increment;
    int dynamic;
    int reallocs;
    int debug;
} strbuf_t;

STRBUF_DEFAULT_SIZE = 16;
STRBUF_DEFAULT_INCREMENT = 16;

int init_both(strbuf_t * s1, strbuf_t * s2) 
// s1.buf => 'a, s2.buf => 'b
{
    strbuf_init(s1, 128);
    strbuf_init(s2, 128);
    return 0;
}

void strbuf_init(strbuf_t *s, int len) 
// s.buf => `a where 'a = alloc()
{
    int size;

    if (len <= 0)
        size = STRBUF_DEFAULT_SIZE;
    else
        size = len + 1;         /* \0 terminator */
    s->buf = NULL;
    s->size = size;
    s->length = 0;
    s->increment = STRBUF_DEFAULT_INCREMENT;
    s->dynamic = 0;
    s->reallocs = 0;
    s->debug = 0;

    s->buf = malloc(size);
    if (!s->buf)
        die("Out of memory");
    //    strbuf_ensure_null(s);
}

void strbuf_free(strbuf_t *s) 
// s.buf => dead
{
   free(s->buf);
}

/*
a => dead

1. a, and everything that it points to, has been freed

2. a has been freed, and everything that is points to is "shared"/accessible 
   from somewhere else
*/