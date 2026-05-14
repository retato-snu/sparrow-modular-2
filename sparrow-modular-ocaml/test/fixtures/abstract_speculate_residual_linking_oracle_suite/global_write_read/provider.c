int shared_g = 0;

int set_shared(void) {
  shared_g = 7;
  return 7;
}

int main(void) {
  return set_shared();
}
