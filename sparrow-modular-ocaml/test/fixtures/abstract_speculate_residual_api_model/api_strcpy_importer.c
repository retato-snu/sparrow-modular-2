extern int api_seed(void);

char *strcpy(char *dst, const char *src) {
  return dst;
}

int main(void) {
  int src = api_seed();
  int dst = 0;
  strcpy((char *)&dst, (char *)&src);
  return dst;
}
