#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static inline double eval_A(int i, int j) {
  return 1.0 / ((i + j) * (i + j + 1) / 2 + i + 1);
}

void eval_A_times_u(int N, const double u[], double Au[]) {
  int i, j;
  for (i = 0; i < N; i++) {
    Au[i] = 0;
    for (j = 0; j < N; j++)
      Au[i] += eval_A(i, j) * u[j];
  }
}

void eval_At_times_u(int N, const double u[], double Au[]) {
  int i, j;
  for (i = 0; i < N; i++) {
    Au[i] = 0;
    for (j = 0; j < N; j++)
      Au[i] += eval_A(j, i) * u[j];
  }
}

// we alloc v and free it at the end of the function, so this function should be total
// () -> `a -> () -> void : total
void eval_AtA_times_u(int N, const double u[], double AtAu[]) {
  double *v = malloc(N * sizeof(double));
  eval_A_times_u(N, u, v);
  eval_At_times_u(N, v, AtAu);
  free(v);
}