extern int provided;

int use_provided(void) {
  int local = provided + 1;
  return local;
}

int main(void) {
  return use_provided();
}
