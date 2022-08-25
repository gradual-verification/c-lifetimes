/// malloc() wrapper
///
/// try_malloc() is a malloc() wrapper that tries to free some memory before
/// trying again.
///
/// @see {try_to_free_memory}
/// @param size
/// @return pointer to allocated space. NULL if out of memory
void *try_malloc(size_t size) FUNC_ATTR_MALLOC FUNC_ATTR_ALLOC_SIZE(1)
{
  size_t allocated_size = size ? size : 1;
  void *ret = malloc(allocated_size);
  if (!ret) {
    try_to_free_memory();
    ret = malloc(allocated_size);
  }
  return ret;
}