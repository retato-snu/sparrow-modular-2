typedef unsigned int size_t;
extern int api_seed(void);

void *memcpy(void *dst, const void *src, size_t n) {
  return dst;
}

int main(void) {
  int src = api_seed();
  int dst = 0;
  memcpy(&dst, &src, sizeof(src));
  return dst;
}
