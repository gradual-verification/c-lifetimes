#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct tn {
  struct tn *left;
  struct tn *right;
  long item;
} treeNode;

treeNode *NewTreeNode(treeNode *left, treeNode *right, long item) {
  treeNode *new;

  new = (treeNode *)malloc(sizeof(treeNode));

  new->left = left;
  new->right = right;
  new->item = item;

  return new;
} /* NewTreeNode() */

void DeleteTree(treeNode *tree) {
  if (tree->left != NULL) {
    DeleteTree(tree->left);
    DeleteTree(tree->right);
  }

  free(tree);
} /* DeleteTree() */