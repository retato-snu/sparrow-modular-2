int g;
int setg(int x) { g = x; return g; }
int main(void) { int y = setg(4); return y; }
