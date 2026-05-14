extern int cycle_a(void);
int cycle_b(void) { int observed = cycle_a(); return 2; }
int main(void) { return cycle_b(); }
