extern int cycle_b(void);
int cycle_a(void) { int observed = cycle_b(); return 1; }
int main(void) { return cycle_a(); }
