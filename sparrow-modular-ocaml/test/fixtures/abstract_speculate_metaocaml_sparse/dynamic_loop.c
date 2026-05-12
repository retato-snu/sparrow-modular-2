extern int nondet(void);

int main(void) {
  int n = nondet();
  int i = 0;
  int acc = 0;
  while (i < n) {
    acc = acc + i;
    i = i + 1;
  }
  return acc;
}
