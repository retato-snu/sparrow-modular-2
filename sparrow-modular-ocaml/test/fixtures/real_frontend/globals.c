int g = 3;
extern int h;

int read_g(void) {
  return g;
}

int main(void) {
  return read_g();
}
