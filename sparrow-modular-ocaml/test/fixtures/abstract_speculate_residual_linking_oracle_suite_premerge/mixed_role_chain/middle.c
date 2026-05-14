extern int base_provider(void);

int middle_provider(void) {
  int observed = base_provider();
  return 7;
}

