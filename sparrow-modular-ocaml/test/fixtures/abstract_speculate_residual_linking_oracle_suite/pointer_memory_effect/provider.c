int write_ptr(int *p) {
  *p = 5;
  return 5;
}

int main(void) {
  int x = 0;
  return write_ptr(&x);
}
