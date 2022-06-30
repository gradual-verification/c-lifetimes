#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char boolean;

static unsigned int nsieve(int m) {
  unsigned int count = 0, i, j;
  boolean *flags = (boolean *)malloc(m * sizeof(boolean));
  memset(flags, 1, m);

  for (i = 2; i < m; ++i)
    if (flags[i]) {
      ++count;
      for (j = i << 1; j < m; j += i)
        if (flags[j])
          flags[j] = 0;
    }

  free(flags);
  return count;
}