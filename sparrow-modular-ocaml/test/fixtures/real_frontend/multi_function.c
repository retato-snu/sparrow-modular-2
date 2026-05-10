int callee(int x) {
  return x + 1;
}

int caller(void) {
  return callee(2);
}

int main(void) {
  return caller();
}
