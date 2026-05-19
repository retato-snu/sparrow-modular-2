int shared_g = 0;

int linked_provider(void) {
  shared_g = 41;
  return 41;
}

int main(void) {
  return linked_provider();
}
