extern int bump_linked(int x);
int main(void) { int i = 0; int acc = 0; while (i < 4) { acc = bump_linked(acc); i = i + 1; } return acc; }
