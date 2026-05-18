extern int cycle_b(void);
int cycle_a_sink;
int cycle_a(void) { int observed = cycle_b(); cycle_a_sink = 0; return 1; }
int main(void) { return cycle_a(); }
