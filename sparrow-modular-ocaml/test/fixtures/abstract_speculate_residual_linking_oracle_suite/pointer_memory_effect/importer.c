extern int write_ptr(int *p);

int main(void) {
  int x = 0;
  int y = write_ptr(&x);
  return y;
}
