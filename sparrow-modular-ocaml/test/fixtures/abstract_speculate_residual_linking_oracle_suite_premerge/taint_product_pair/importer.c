extern int taint_source(void);

int main(void) {
  int x = taint_source();
  if (x == 42) return x;
  return 0;
}
