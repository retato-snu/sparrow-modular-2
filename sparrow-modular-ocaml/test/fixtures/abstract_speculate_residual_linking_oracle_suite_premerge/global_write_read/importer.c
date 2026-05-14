extern int shared_g;
extern int set_shared(void);

int main(void) {
  int x = set_shared();
  return x + shared_g;
}
