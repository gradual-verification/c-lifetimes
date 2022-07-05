#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct tn {
  struct tn *left;
  struct tn *right;
  long item;
} treeNode;

// 'a -> 'a -> () -> `a
treeNode *NewTreeNode(treeNode *left, treeNode *right, long item) {
  treeNode *new;

  new = (treeNode *)malloc(sizeof(treeNode));

  new->left = left;
  new->right = right;
  new->item = item;

  return new;
} /* NewTreeNode() */

/* One option: a recursive predicate like the following:
predicate P {
  if (root) {
    (root => dead) ^ P(root->left) ^ P(root->right)
  } else {
    true
  }
}
*/
/* Alternatively, we can give a more liberal definition of what it means to rewrite to "dead."
   Under this interpretation, a pointer variable rewrites to dead if:
    - its data is freed
    - anything that it transitively points to is either freed or owned by something else (runtime check or precondition 
      if owner is outside current function)
  `a => dead -> void
*/
void DeleteTree(treeNode *tree) {
  if (tree->left != NULL) {
    DeleteTree(tree->left);
    DeleteTree(tree->right);
  }

  free(tree);
} /* DeleteTree() */