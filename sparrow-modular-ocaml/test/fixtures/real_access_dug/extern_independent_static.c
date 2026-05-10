extern int ext(void);
int local(int x) { return x + 2; }
int main(void) { int x = 1; int y = local(x); return y; }
