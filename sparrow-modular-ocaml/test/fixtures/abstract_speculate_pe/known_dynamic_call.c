extern int ext(void);

int identity(int x) {
  return x;
}

int main(void) {
  int x = ext();
  int y = identity(x);
  return y;
}
