extern int shared_g;
extern void set_shared(void);
int main(void) { set_shared(); return shared_g; }
