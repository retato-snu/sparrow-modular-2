extern int cycle_value_left(void);
int cycle_value_right_sink;
int cycle_value_right(void) { int observed = cycle_value_left(); cycle_value_right_sink = observed; return 4; }
int main(void) { return cycle_value_right(); }
