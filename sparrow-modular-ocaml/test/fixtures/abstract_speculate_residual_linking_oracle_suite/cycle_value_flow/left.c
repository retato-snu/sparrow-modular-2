extern int cycle_value_right(void);
int cycle_value_left_sink;
int cycle_value_left(void) { int observed = cycle_value_right(); cycle_value_left_sink = observed; return 3; }
int main(void) { return cycle_value_left(); }
