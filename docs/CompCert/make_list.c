#include <stdio.h>
#include <stdlib.h>

#define NDATA (int *)malloc(ncol * sizeof(int))
#define NLIST (struct _list *)malloc(sizeof(struct _list))
#define NPLAY (struct _play *)malloc(sizeof(struct _play))

int ncol = 7;
int nrow = 7;

struct _list {
  int *data;
  struct _list *next;
} * wanted;

// returns a linked list node
// `a -> `b -> `c -> `d : alloc<`d>
struct _list *make_list(int *data, int *value,
                        int *all) /* create the whole _list of moves */
                                  /* for the _data structure data */
{
  int row, col;
  int *temp;
  struct _list *head, *current;
  *value = 1;                       /* set to not good to give */
  head = NLIST;                     /* create dummy header */
  head->next = NULL;                /* set NULL as next element */
  current = head;                   /* start from here */
  for (row = 0; row != nrow; row++) /* for every row */
  {
    for (col = 0; col != ncol; col++) /* for every column */
    {
      temp = make_data(row, col);  /* create _data for this play */
      melt_data(temp, data);       /* melt it with the current one */
      if (!equal_data(temp, data)) /* if they are different, it good */
      {
        current->next = NLIST; /* create new element in list */
        current->next->data =
            copy_data(temp);            /* copy data, and place in list */
        current->next->next = NULL;     /* NULL the next element */
        current = current->next;        /* advance pointer */
        if (*value == 1)                /* if still not found a good one */
          *value = get_value(temp);     /* look at this value */
        if ((!*all) && (*value == 0)) { /* if we found it, and all is not set */
          col = ncol - 1;               /* do what it take sto break out now */
          row = nrow - 1;
          if (in_wanted(temp)) /* if in the wanted list */
            *all = 2;          /* flag it */
        }
      } else /* if its not a valid move */
      {
        if (col == 0)
          row = nrow - 1; /* break out if at first column */
        col = ncol - 1;   /* but make sure you break out */
      }                   /* of the col for-loop anyway */
      free(temp);         /* dump this unneeded space */
    }
  }
  current = head->next; /* skip first element */
  free(head);           /* dump it */
  if (current != NULL)
    *value = 1 - *value; /* invert value if its */
  return current;        /* not the empty board */
}