extern int scheduler_a(void);
int scheduler_b_sink;
int scheduler_b(void) { int observed = scheduler_a(); scheduler_b_sink = observed; return 20; }
int main(void) { return scheduler_b(); }
