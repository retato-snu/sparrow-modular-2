extern int scheduler_b(void);
int scheduler_a_sink;
int scheduler_a(void) { int observed = scheduler_b(); scheduler_a_sink = observed; return 10; }
int main(void) { return scheduler_a(); }
