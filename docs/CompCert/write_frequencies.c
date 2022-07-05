#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct ht_node {
  char *key;
  int val;
  struct ht_node *next;
};

struct ht_ht {
  int size;
  struct ht_node **tbl;
  int iter_index;
  struct ht_node *iter_next;
  int items;
#ifdef HT_DEBUG
  int collisions;
#endif /* HT_DEBUG */
};

typedef struct ssorter {
  char *string;
  int num;
} sorter;

// total
// () -> `a -> () -> void : total
void write_frequencies(int fl, char *buffer, long buflen) {
  struct ht_ht *ht;
  long total, i, j, size;
  struct ht_node *nd;
  sorter *s;
  sorter tmp;

  ht = generate_frequencies(fl, buffer, buflen);
  total = 0;
  size = 0;
  for (nd = ht_first(ht); nd != NULL; nd = ht_next(ht)) {
    total = total + nd->val;
    size++;
  }
  s = calloc(size, sizeof(sorter));
  i = 0;
  for (nd = ht_first(ht); nd != NULL; nd = ht_next(ht)) {
    s[i].string = nd->key;
    s[i++].num = nd->val;
  }
  for (i = 0; i < size - 1; i++)
    for (j = i + 1; j < size; j++)
      if (s[i].num < s[j].num) {
        memcpy(&tmp, &(s[i]), sizeof(sorter));
        memcpy(&(s[i]), &(s[j]), sizeof(sorter));
        memcpy(&(s[j]), &tmp, sizeof(sorter));
      }
  for (i = 0; i < size; i++)
    printf("%s %.3f\n", s[i].string, 100 * (float)s[i].num / total);
  printf("\n");
  ht_destroy(ht);
  free(s);
}