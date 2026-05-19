typedef unsigned int size_t;
extern int api_seed(void);

size_t strlen(const char *src) {
  return 0;
}

int main(void) {
  int src = api_seed();
  int n = (int)strlen((char *)&src);
  return n;
}
