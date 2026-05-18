extern int cycle_a(void);
int cycle_b_sink;
int cycle_b(void) { int observed = cycle_a(); cycle_b_sink = observed; return 2; }
int main(void) { return cycle_b(); }
